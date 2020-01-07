/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.GemfireUtils.apacheGeodeProductName;
import static org.springframework.data.gemfire.GemfireUtils.apacheGeodeVersion;
import static org.springframework.data.gemfire.support.GemfireBeanFactoryLocator.newBeanFactoryLocator;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import org.apache.geode.GemFireCheckedException;
import org.apache.geode.GemFireException;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.DynamicRegionFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.util.GatewayConflictResolver;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.internal.datasource.ConfigProperty;
import org.apache.geode.internal.jndi.JNDIInvoker;
import org.apache.geode.pdx.PdxSerializable;
import org.apache.geode.pdx.PdxSerializer;
import org.apache.geode.security.SecurityManager;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.Phased;
import org.springframework.core.io.Resource;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.support.PersistenceExceptionTranslator;
import org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.data.gemfire.support.GemfireBeanFactoryLocator;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to construct, configure and initialize a Pivotal GemFire/Apache Geode
 * {@link Cache peer cache).
 *
 * Allows either retrieval of an existing, open {@link Cache} or creation of a new {@link Cache}.
 *
 * This class implements the {@link PersistenceExceptionTranslator} interface and is auto-detected by Spring's
 * {@link org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor} for AOP-based translation
 * of native persistent data store exceptions to Spring's {@link DataAccessException} hierarchy. Therefore, the presence
 * of this class automatically enables a
 * {@link org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor}
 * to translate Pivotal GemFire/Apache Geode exceptions appropriately.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see java.util.Properties
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.DynamicRegionFactory
 * @see org.apache.geode.cache.RegionService
 * @see org.apache.geode.distributed.DistributedMember
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.apache.geode.cache.pdx.PdxSerializer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.context.Phased
 * @see org.springframework.core.io.Resource
 * @see org.springframework.dao.support.PersistenceExceptionTranslator
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
 */
@SuppressWarnings("unused")
public class CacheFactoryBean extends AbstractFactoryBeanSupport<GemFireCache>
		implements DisposableBean, InitializingBean, PersistenceExceptionTranslator, Phased {

	private boolean close = true;
	private boolean useBeanFactoryLocator = false;

	private int phase = -1;

	private Boolean copyOnRead;
	private Boolean enableAutoReconnect;
	private Boolean pdxIgnoreUnreadFields;
	private Boolean pdxPersistent;
	private Boolean pdxReadSerialized;
	private Boolean useClusterConfiguration;

	private CacheFactoryInitializer<?> cacheFactoryInitializer;

	private GemFireCache cache;

	private DynamicRegionSupport dynamicRegionSupport;

	private Float criticalHeapPercentage;
	private Float criticalOffHeapPercentage;
	private Float evictionHeapPercentage;
	private Float evictionOffHeapPercentage;

	private GatewayConflictResolver gatewayConflictResolver;

	protected GemfireBeanFactoryLocator beanFactoryLocator;

	private Integer lockLease;
	private Integer lockTimeout;
	private Integer messageSyncInterval;
	private Integer searchTimeout;

	private List<PeerCacheConfigurer> peerCacheConfigurers = new ArrayList<>();

	private List<JndiDataSource> jndiDataSources;

	private List<TransactionListener> transactionListeners;

	private PdxSerializer pdxSerializer;

	private PeerCacheConfigurer compositePeerCacheConfigurer = (beanName, bean) ->
		nullSafeList(peerCacheConfigurers).forEach(peerCacheConfigurer ->
			peerCacheConfigurer.configure(beanName, bean));

	private Properties properties;

	private Resource cacheXml;

	private String cacheResolutionMessagePrefix;
	private String pdxDiskStoreName;

	private org.apache.geode.security.SecurityManager securityManager;

	private TransactionWriter transactionWriter;

	/**
	 * Initializes this {@link CacheFactoryBean} after properties have been set by the Spring container.
	 *
	 * @throws Exception if initialization fails.
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 * @see #applyCacheConfigurers()
	 * @see #initBeanFactoryLocator()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		applyCacheConfigurers();
		initBeanFactoryLocator();
	}

	/**
	 * Applies the composite {@link PeerCacheConfigurer PeerCacheConfigurers} to this {@link CacheFactoryBean}
	 * before creating the {@link Cache peer Cache}.
	 *
	 * @see #getCompositePeerCacheConfigurer()
	 * @see #applyPeerCacheConfigurers(PeerCacheConfigurer...)
	 */
	protected void applyCacheConfigurers() {

		PeerCacheConfigurer autoReconnectClusterConfigurationConfigurer = (beanName, cacheFactoryBean) -> {

			Properties gemfireProperties = resolveProperties();

			gemfireProperties.setProperty("disable-auto-reconnect",
				String.valueOf(!Boolean.TRUE.equals(getEnableAutoReconnect())));

			gemfireProperties.setProperty("use-cluster-configuration",
				String.valueOf(Boolean.TRUE.equals(getUseClusterConfiguration())));
		};

		this.peerCacheConfigurers.add(autoReconnectClusterConfigurationConfigurer);

		applyPeerCacheConfigurers(getCompositePeerCacheConfigurer());
	}

	/**
	 * Applies the given array of {@link PeerCacheConfigurer PeerCacheConfigurers} to this {@link CacheFactoryBean}.
	 *
	 * @param peerCacheConfigurers array of {@link PeerCacheConfigurer PeerCacheConfigurers} applied to
	 * this {@link CacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 * @see #applyPeerCacheConfigurers(Iterable)
	 */
	protected void applyPeerCacheConfigurers(PeerCacheConfigurer... peerCacheConfigurers) {
		applyPeerCacheConfigurers(Arrays.asList(nullSafeArray(peerCacheConfigurers, PeerCacheConfigurer.class)));
	}

	/**
	 * Applies the given {@link Iterable} of {@link PeerCacheConfigurer PeerCacheConfigurers}
	 * to this {@link CacheFactoryBean}.
	 *
	 * @param peerCacheConfigurers {@link Iterable} of {@link PeerCacheConfigurer PeerCacheConfigurers}
	 * applied to this {@link CacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 * @see java.lang.Iterable
	 * @see #applyPeerCacheConfigurers(PeerCacheConfigurer...)
	 */
	protected void applyPeerCacheConfigurers(Iterable<PeerCacheConfigurer> peerCacheConfigurers) {
		stream(nullSafeIterable(peerCacheConfigurers).spliterator(), false)
			.forEach(clientCacheConfigurer -> clientCacheConfigurer.configure(getBeanName(), this));
	}

	/**
	 * Initializes the {@link GemfireBeanFactoryLocator} if {@link #isUseBeanFactoryLocator()} returns {@literal true}
	 * and an existing {@link #getBeanFactoryLocator()} is not already present.
	 *
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator#newBeanFactoryLocator(BeanFactory, String)
	 * @see #isUseBeanFactoryLocator()
	 * @see #getBeanFactoryLocator()
	 * @see #getBeanFactory()
	 * @see #getBeanName()
	 */
	void initBeanFactoryLocator() {

		if (isUseBeanFactoryLocator() && this.beanFactoryLocator == null) {
			this.beanFactoryLocator = newBeanFactoryLocator(getBeanFactory(), getBeanName());
		}
	}

	/**
	 * Initializes the {@link Cache}.
	 *
	 * @return a reference to the initialized {@link Cache}.
	 * @see org.apache.geode.cache.Cache
	 * @see #resolveCache()
	 * @see #postProcess(GemFireCache)
	 * @see #setCache(GemFireCache)
	 */
	@SuppressWarnings("deprecation")
	GemFireCache init() {

		ClassLoader currentThreadContextClassLoader = Thread.currentThread().getContextClassLoader();

		try {
			// Use Spring Bean ClassLoader to load Spring configured, Pivotal GemFire/Apache Geode classes
			Thread.currentThread().setContextClassLoader(getBeanClassLoader());

			setCache(postProcess(resolveCache()));

			Optional.<GemFireCache>ofNullable(getCache()).ifPresent(cache -> {

				Optional.ofNullable(cache.getDistributedSystem())
					.map(DistributedSystem::getDistributedMember)
					.ifPresent(member ->
						logInfo(() -> String.format("Connected to Distributed System [%1$s] as Member [%2$s]"
								.concat(" in Group(s) [%3$s] with Role(s) [%4$s] on Host [%5$s] having PID [%6$d]"),
							cache.getDistributedSystem().getName(), member.getId(), member.getGroups(),
							member.getRoles(), member.getHost(), member.getProcessId())));

				logInfo(() -> String.format("%1$s %2$s version [%3$s] Cache [%4$s]", this.cacheResolutionMessagePrefix,
					apacheGeodeProductName(), apacheGeodeVersion(), cache.getName()));

			});

			return getCache();
		}
		catch (Exception cause) {
			throw newRuntimeException(cause, "Error occurred when initializing peer cache");
		}
		finally {
			Thread.currentThread().setContextClassLoader(currentThreadContextClassLoader);
		}
	}

	/**
	 * Resolves the {@link Cache} by first attempting to lookup an existing {@link Cache} instance in the JVM.
	 * If an existing {@link Cache} could not be found, then this method proceeds in attempting to create
	 * a new {@link Cache} instance.
	 *
	 * @param <T> parameterized {@link Class} type extension of {@link GemFireCache}.
	 * @return the resolved {@link Cache} instance.
	 * @see org.apache.geode.cache.Cache
	 * @see #fetchCache()
	 * @see #resolveProperties()
	 * @see #createFactory(java.util.Properties)
	 * @see #configureFactory(Object)
	 * @see #createCache(Object)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T resolveCache() {

		try {

			this.cacheResolutionMessagePrefix = "Found existing";

			return (T) fetchCache();
		}
		catch (CacheClosedException cause) {

			this.cacheResolutionMessagePrefix = "Created new";
			initDynamicRegionFactory();

			return (T) createCache(postProcess(configureFactory(initializeFactory(createFactory(resolveProperties())))));
		}
	}

	/**
	 * Fetches an existing {@link Cache} instance from the {@link CacheFactory}.
	 *
	 * @param <T> parameterized {@link Class} type extension of {@link GemFireCache}.
	 * @return an existing {@link Cache} instance if available.
	 * @throws org.apache.geode.cache.CacheClosedException if an existing {@link Cache} instance does not exist.
	 * @see org.apache.geode.cache.CacheFactory#getAnyInstance()
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #getCache()
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		return (T) Optional.ofNullable(getCache()).orElseGet(CacheFactory::getAnyInstance);
	}

	/**
	 * If Pivotal GemFire/Apache Geode Dynamic Regions are enabled, create and initialize a {@link DynamicRegionFactory}
	 * before creating the {@link Cache}.
	 *
	 * @see org.springframework.data.gemfire.CacheFactoryBean.DynamicRegionSupport#initDynamicRegionFactory()
	 * @see #getDynamicRegionSupport()
	 */
	private void initDynamicRegionFactory() {
		Optional.ofNullable(getDynamicRegionSupport()).ifPresent(DynamicRegionSupport::initializeDynamicRegionFactory);
	}

	/**
	 * Resolves the Pivotal GemFire/Apache Geode {@link Properties} used to configure the {@link Cache}.
	 *
	 * @return the resolved Pivotal GemFire/Apache Geode {@link Properties} used to configure the {@link Cache}.
	 * @see #setAndGetProperties(Properties)
	 * @see #getProperties()
	 */
	protected Properties resolveProperties() {
		return Optional.ofNullable(getProperties()).orElseGet(() -> setAndGetProperties(new Properties()));
	}

	/**
	 * Constructs a new instance of {@link CacheFactory} initialized with the given Pivotal GemFire/Apache Geode
	 * {@link Properties} used to construct, configure and initialize an instance of a {@link Cache}.
	 *
	 * @param gemfireProperties {@link Properties} used by the {@link CacheFactory} to configure the {@link Cache}.
	 * @return a new instance of {@link CacheFactory} initialized with the given Pivotal GemFire/Apache Geode
	 * {@link Properties}.
	 * @see org.apache.geode.cache.CacheFactory
	 * @see java.util.Properties
	 */
	protected Object createFactory(Properties gemfireProperties) {
		return new CacheFactory(gemfireProperties);
	}

	/**
	 * Initializes the given {@link CacheFactory} with the configured {@link CacheFactoryInitializer}.
	 *
	 * @param factory {@link CacheFactory} to initialize; may be {@literal null}.
	 * @return the initialized {@link CacheFactory}.
	 * @see org.springframework.data.gemfire.CacheFactoryBean.CacheFactoryInitializer#initialize(Object)
	 * @see org.apache.geode.cache.CacheFactory
	 * @see #getCacheFactoryInitializer()
	 */
	@Nullable
	@SuppressWarnings("unchecked")
	protected Object initializeFactory(Object factory) {

		return Optional.ofNullable(getCacheFactoryInitializer())
			.map(cacheFactoryInitializer -> cacheFactoryInitializer.initialize(factory))
			.orElse(factory);
	}

	/**
	 * Configures the {@link CacheFactory} used to create the {@link Cache}.
	 *
	 * Sets PDX options specified by the user.
	 *
	 * @param factory {@link CacheFactory} used to create the {@link Cache}.
	 * @return the configured {@link CacheFactory}.
	 * @see org.apache.geode.cache.CacheFactory
	 * @see #configurePdx(CacheFactory)
	 */
	protected Object configureFactory(Object factory) {
		return configureSecurity(configurePdx((CacheFactory) factory));
	}

	/**
	 * Configures PDX for this peer {@link Cache} instance.
	 *
	 * @param cacheFactory {@link CacheFactory} used to configure the peer {@link Cache} with PDX.
	 * @return the given {@link CacheFactory}.
	 * @see org.apache.geode.cache.CacheFactory
	 */
	private CacheFactory configurePdx(CacheFactory cacheFactory) {

		Optional.ofNullable(getPdxSerializer()).ifPresent(cacheFactory::setPdxSerializer);

		Optional.ofNullable(getPdxDiskStoreName()).filter(StringUtils::hasText)
			.ifPresent(cacheFactory::setPdxDiskStore);

		Optional.ofNullable(getPdxIgnoreUnreadFields()).ifPresent(cacheFactory::setPdxIgnoreUnreadFields);

		Optional.ofNullable(getPdxPersistent()).ifPresent(cacheFactory::setPdxPersistent);

		Optional.ofNullable(getPdxReadSerialized()).ifPresent(cacheFactory::setPdxReadSerialized);

		return cacheFactory;
	}

	/**
	 * Configures security for this peer {@link Cache} instance.
	 *
	 * @param cacheFactory {@link CacheFactory} used to configure the peer {@link Cache} with security.
	 * @return the given {@link CacheFactory}.
	 * @see org.apache.geode.cache.CacheFactory
	 */
	private CacheFactory configureSecurity(CacheFactory cacheFactory) {

		Optional.ofNullable(getSecurityManager()).ifPresent(cacheFactory::setSecurityManager);

		return cacheFactory;
	}

	/**
	 * Post processes the {@link CacheFactory} used to create the {@link Cache}.
	 *
	 * @param factory {@link CacheFactory} used to create the {@link Cache}.
	 * @return the post processed {@link CacheFactory}.
	 * @see org.apache.geode.cache.CacheFactory
	 */
	protected Object postProcess(Object factory) {
		return factory;
	}

	/**
	 * Creates a new {@link Cache} instance using the provided {@link Object factory}.
	 *
	 * @param <T> {@link Class sub-type} of {@link GemFireCache}.
	 * @param factory instance of {@link CacheFactory}.
	 * @return a new instance of {@link Cache} created by the provided {@link Object factory}.
	 * @see org.apache.geode.cache.CacheFactory#create()
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T createCache(Object factory) {
		return (T) ((CacheFactory) factory).create();
	}

	/**
	 * Post processes the {@link GemFireCache} by loading any {@literal cache.xml}, applying custom settings
	 * specified in SDG XML configuration meta-data, and registering appropriate Transaction Listeners, Writer
	 * and JNDI settings.
	 *
	 * @param <T> Parameterized {@link Class} type extension of {@link GemFireCache}.
	 * @param cache {@link GemFireCache} instance to post process.
	 * @return the given {@link GemFireCache}.
	 * @see org.apache.geode.cache.Cache#loadCacheXml(java.io.InputStream)
	 * @see #getCacheXml()
	 * @see #configureHeapPercentages(org.apache.geode.cache.GemFireCache)
	 * @see #registerJndiDataSources()
	 * @see #registerTransactionListeners(org.apache.geode.cache.GemFireCache)
	 * @see #registerTransactionWriter(org.apache.geode.cache.GemFireCache)
	 */
	@SuppressWarnings("all")
	protected <T extends GemFireCache> T postProcess(T cache) {

		loadCacheXml(cache);

		Optional.ofNullable(getCopyOnRead()).ifPresent(cache::setCopyOnRead);

		if (cache instanceof Cache) {
			Optional.ofNullable(getGatewayConflictResolver()).ifPresent(((Cache) cache)::setGatewayConflictResolver);
			Optional.ofNullable(getLockLease()).ifPresent(((Cache) cache)::setLockLease);
			Optional.ofNullable(getLockTimeout()).ifPresent(((Cache) cache)::setLockTimeout);
			Optional.ofNullable(getMessageSyncInterval()).ifPresent(((Cache) cache)::setMessageSyncInterval);
			Optional.ofNullable(getSearchTimeout()).ifPresent(((Cache) cache)::setSearchTimeout);
		}

		configureHeapPercentages(cache);
		configureOffHeapPercentages(cache);
		registerJndiDataSources(cache);
		registerTransactionListeners(cache);
		registerTransactionWriter(cache);

		return cache;
	}

	private <T extends GemFireCache> T loadCacheXml(T cache) {

		// Load cache.xml Resource and initialize the cache
		Optional.ofNullable(getCacheXml()).ifPresent(cacheXml -> {
			try {
				logDebug("Initializing cache with [%s]", cacheXml);
				cache.loadCacheXml(cacheXml.getInputStream());
			}
			catch (IOException cause) {
				throw newRuntimeException(cause, "Failed to load cache.xml [%s]", cacheXml);
			}
		});

		return cache;
	}

	private boolean isHeapPercentageValid(Float heapPercentage) {
		return heapPercentage >= 0.0f && heapPercentage <= 100.0f;
	}

	private GemFireCache configureHeapPercentages(GemFireCache cache) {

		Optional.ofNullable(getCriticalHeapPercentage()).ifPresent(criticalHeapPercentage -> {

			Assert.isTrue(isHeapPercentageValid(criticalHeapPercentage), String.format(
				"criticalHeapPercentage [%s] is not valid; must be >= 0.0 and <= 100.0", criticalHeapPercentage));

			cache.getResourceManager().setCriticalHeapPercentage(criticalHeapPercentage);
		});

		Optional.ofNullable(getEvictionHeapPercentage()).ifPresent(evictionHeapPercentage -> {

			Assert.isTrue(isHeapPercentageValid(evictionHeapPercentage), String.format(
				"evictionHeapPercentage [%s] is not valid; must be >= 0.0 and <= 100.0", evictionHeapPercentage));

			cache.getResourceManager().setEvictionHeapPercentage(evictionHeapPercentage);
		});

		return cache;
	}

	private GemFireCache configureOffHeapPercentages(GemFireCache cache) {

		Optional.ofNullable(getCriticalOffHeapPercentage()).ifPresent(criticalOffHeapPercentage -> {

			Assert.isTrue(isHeapPercentageValid(criticalOffHeapPercentage), String.format(
				"criticalOffHeapPercentage [%s] is not valid; must be >= 0.0 and <= 100.0", criticalOffHeapPercentage));

			cache.getResourceManager().setCriticalOffHeapPercentage(criticalOffHeapPercentage);
		});

		Optional.ofNullable(getEvictionOffHeapPercentage()).ifPresent(evictionOffHeapPercentage -> {

			Assert.isTrue(isHeapPercentageValid(evictionOffHeapPercentage), String.format(
				"evictionOffHeapPercentage [%s] is not valid; must be >= 0.0 and <= 100.0", evictionOffHeapPercentage));

			cache.getResourceManager().setEvictionOffHeapPercentage(evictionOffHeapPercentage);
		});

		return cache;
	}

	private GemFireCache registerJndiDataSources(GemFireCache cache) {

		nullSafeCollection(getJndiDataSources()).forEach(jndiDataSource -> {

			String type = jndiDataSource.getAttributes().get("type");

			JndiDataSourceType jndiDataSourceType = JndiDataSourceType.valueOfIgnoreCase(type);

			Assert.notNull(jndiDataSourceType,
				String.format("'jndi-binding' 'type' [%1$s] is invalid; 'type' must be one of %2$s",
					type, Arrays.toString(JndiDataSourceType.values())));

			jndiDataSource.getAttributes().put("type", jndiDataSourceType.getName());

			SpringUtils.safeRunOperation(() ->
				JNDIInvoker.mapDatasource(jndiDataSource.getAttributes(), jndiDataSource.getProps()));
		});

		return cache;
	}

	private GemFireCache registerTransactionListeners(GemFireCache cache) {

		nullSafeCollection(getTransactionListeners())
			.forEach(transactionListener -> cache.getCacheTransactionManager().addListener(transactionListener));

		return cache;
	}

	private GemFireCache registerTransactionWriter(GemFireCache cache) {

		Optional.ofNullable(getTransactionWriter()).ifPresent(it -> cache.getCacheTransactionManager().setWriter(it));

		return cache;
	}

	/**
	 * Null-safe internal method used to close the {@link GemFireCache} and calling {@link GemFireCache#close()}
	 * iff the cache {@link GemFireCache#isClosed() is not already closed}.
	 *
	 * @param cache {@link GemFireCache} to close.
	 * @see org.apache.geode.cache.GemFireCache#isClosed()
	 * @see org.apache.geode.cache.GemFireCache#close()
	 */
	protected void close(GemFireCache cache) {

		Optional.ofNullable(cache)
			.filter(it -> !it.isClosed())
			.ifPresent(RegionService::close);

		setCache(null);
	}

	/**
	 * Destroys the {@link Cache} bean on Spring container shutdown.
	 *
	 * @throws Exception if an error occurs while closing the cache.
	 * @see org.springframework.beans.factory.DisposableBean#destroy()
	 * @see #destroyBeanFactoryLocator()
	 * @see #close(GemFireCache)
	 * @see #isClose()
	 */
	@Override
	public void destroy() throws Exception {

		if (isClose()) {
			close(fetchCache());
			destroyBeanFactoryLocator();
		}
	}

	/**
	 * Destroys the {@link GemfireBeanFactoryLocator}.
	 *
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator#destroy()
	 */
	private void destroyBeanFactoryLocator() {
		Optional.ofNullable(getBeanFactoryLocator()).ifPresent(GemfireBeanFactoryLocator::destroy);
		this.beanFactoryLocator = null;
	}

	/**
	 * Translates the given Pivotal GemFire/Apache Geode {@link RuntimeException} thrown to a corresponding exception
	 * from Spring's generic {@link DataAccessException} hierarchy, if possible.
	 *
	 * @param exception {@link RuntimeException} to translate.
	 * @return the translated Spring {@link DataAccessException} or {@literal null} if the Pivotal GemFire/Apache Geode
	 * {@link RuntimeException} could not be converted.
	 * @see org.springframework.dao.support.PersistenceExceptionTranslator#translateExceptionIfPossible(RuntimeException)
	 * @see org.springframework.dao.DataAccessException
	 */
	@Override
	@SuppressWarnings("all")
	public DataAccessException translateExceptionIfPossible(RuntimeException exception) {

		if (exception instanceof IllegalArgumentException) {

			DataAccessException wrapped = GemfireCacheUtils.convertQueryExceptions(exception);

			// ignore conversion if generic exception is returned
			if (!(wrapped instanceof GemfireSystemException)) {
				return wrapped;
			}
		}

		if (exception instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) exception);
		}

		if (exception.getCause() instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) exception.getCause());
		}

		if (exception.getCause() instanceof GemFireCheckedException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireCheckedException) exception.getCause());
		}

		return null;
	}

	/**
	 * Returns a reference to the configured {@link GemfireBeanFactoryLocator} used to resolve Spring bean references
	 * in native Pivotal GemFire/Apache Geode native config (e.g. {@literal cache.xml}).
	 *
	 * @return a reference to the configured {@link GemfireBeanFactoryLocator}.
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
	 */
	public GemfireBeanFactoryLocator getBeanFactoryLocator() {
		return this.beanFactoryLocator;
	}

	/**
	 * Sets a reference to the constructed, configured an initialized {@link Cache}
	 * created by this {@link CacheFactoryBean}.
	 *
	 * @param cache {@link Cache} created by this {@link CacheFactoryBean}.
	 * @see org.apache.geode.cache.Cache
	 */
	protected void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	/**
	 * Returns a direct reference to the constructed, configured an initialized {@link Cache}
	 * created by this {@link CacheFactoryBean}.
	 *
	 * @return a direct reference to the {@link Cache} created by this {@link CacheFactoryBean}.
	 * @see org.apache.geode.cache.Cache
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T getCache() {
		return (T) this.cache;
	}

	/**
	 * Sets a reference to the Pivotal GemFire/Apache Geode native {@literal cache.xml} {@link Resource}.
	 *
	 * @param cacheXml reference to the Pivotal GemFire/Apache Geode native {@literal cache.xml} {@link Resource}.
	 * @see org.springframework.core.io.Resource
	 */
	public void setCacheXml(Resource cacheXml) {
		this.cacheXml = cacheXml;
	}

	/**
	 * Returns a reference to the Pivotal GemFire/Apache Geode native {@literal cache.xml}
	 * as a Spring {@link Resource}.
	 *
	 * @return a reference to the Pivotal GemFire/Apache Geode native {@literal cache.xml}
	 * as a Spring {@link Resource}.
	 * @see org.springframework.core.io.Resource
	 */
	public Resource getCacheXml() {
		return this.cacheXml;
	}

	/**
	 * Returns the {@literal cache.xml} {@link Resource} as a {@link File}.
	 *
	 * @return the {@literal cache.xml} {@link Resource} as a {@link File}.
	 * @throws IllegalStateException if the {@link Resource} is not a valid {@link File} in the file system
	 * or a general problem exists accessing or reading the {@link File}.
	 * @see org.springframework.core.io.Resource
	 * @see java.io.File
	 * @see #getCacheXml()
	 */
	private File getCacheXmlFile() {

		try {
			return getCacheXml().getFile();
		}
		catch (Throwable cause) {
			throw newIllegalStateException(cause, "Resource [%s] is not resolvable as a file", getCacheXml());
		}
	}

	/**
	 * Determines whether the {@link Resource cache.xml} {@link File} is present.
	 *
	 * @return boolean value indicating whether a {@link Resource cache.xml} {@link File} is present.
	 * @see #getCacheXmlFile()
	 */
	@SuppressWarnings("all")
	private boolean isCacheXmlAvailable() {

		try {
			return getCacheXmlFile() != null;
		}
		catch (Throwable ignore) {
			return false;
		}
	}

	/**
	 * Returns an object reference to the {@link Cache} created by this {@link CacheFactoryBean}.
	 *
	 * @return an object reference to the {@link Cache} created by this {@link CacheFactoryBean}.
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 * @see org.apache.geode.cache.Cache
	 * @see #getCache()
	 */
	@Override
	@SuppressWarnings("all")
	public GemFireCache getObject() throws Exception {

		return Optional.<GemFireCache>ofNullable(getCache())
			.orElseGet(this::init);
	}

	/**
	 * Returns the {@link Class} type of the {@link GemFireCache} produced by this {@link CacheFactoryBean}.
	 *
	 * @return the {@link Class} type of the {@link GemFireCache} produced by this {@link CacheFactoryBean}.
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public Class<? extends GemFireCache> getObjectType() {
		return Optional.ofNullable(this.<Cache>getCache()).<Class>map(Object::getClass).orElse(Cache.class);
	}

	/**
	 * Set the {@link CacheFactoryInitializer} that will be called to initialize the cache factory used to create
	 * the cache constructed by this {@link CacheFactoryBean}.
	 *
	 * @param cacheFactoryInitializer {@link CacheFactoryInitializer} configured to initialize the cache factory.
	 * @see org.springframework.data.gemfire.CacheFactoryBean.CacheFactoryInitializer
	 */
	public void setCacheFactoryInitializer(CacheFactoryInitializer cacheFactoryInitializer) {
		this.cacheFactoryInitializer = cacheFactoryInitializer;
	}

	/**
	 * Return the {@link CacheFactoryInitializer} that will be called to initialize the cache factory used to create
	 * the cache constructed by this {@link CacheFactoryBean}.
	 *
	 * @return the {@link CacheFactoryInitializer} configured to initialize the cache factory.
	 * @see org.springframework.data.gemfire.CacheFactoryBean.CacheFactoryInitializer
	 */
	public CacheFactoryInitializer getCacheFactoryInitializer() {
		return this.cacheFactoryInitializer;
	}

	/**
	 * Sets and then returns a reference to Pivotal GemFire/Apache Geode {@link Properties} used to configure the cache.
	 *
	 * @param properties reference to Pivotal GemFire/Apache Geode {@link Properties} used to configure the cache.
	 * @return a reference to Pivotal GemFire/Apache Geode {@link Properties} used to configure the cache.
	 * @see java.util.Properties
	 * @see #setProperties(Properties)
	 * @see #getProperties()
	 */
	protected Properties setAndGetProperties(Properties properties) {
		setProperties(properties);
		return getProperties();
	}

	/**
	 * Returns a reference to Pivotal GemFire/Apache Geode {@link Properties} used to configure the cache.
	 *
	 * @param properties reference to Pivotal GemFire/Apache Geode {@link Properties} used to configure the cache.
	 * @see java.util.Properties
	 */
	public void setProperties(Properties properties) {
		this.properties = properties;
	}

	/**
	 * Returns a reference to Pivotal GemFire/Apache Geode {@link Properties} used to configure the cache.
	 *
	 * @return a reference to Pivotal GemFire/Apache Geode {@link Properties}.
	 * @see java.util.Properties
	 */
	public Properties getProperties() {
		return this.properties;
	}

	/**
	 * Sets a value to indicate whether the cache will be closed on shutdown of the Spring container.
	 *
	 * @param close boolean value indicating whether the cache will be closed on shutdown of the Spring container.
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * Returns a boolean value indicating whether the cache will be closed on shutdown of the Spring container.
	 *
	 * @return a boolean value indicating whether the cache will be closed on shutdown of the Spring container.
	 */
	public boolean isClose() {
		return this.close;
	}

	/**
	 * Returns a reference to the Composite {@link PeerCacheConfigurer} used to apply additional configuration
	 * to this {@link CacheFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link PeerCacheConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 */
	public PeerCacheConfigurer getCompositePeerCacheConfigurer() {
		return this.compositePeerCacheConfigurer;
	}

	/**
	 * Set the copyOnRead attribute of the Cache.
	 *
	 * @param copyOnRead a boolean value indicating whether the object stored in the Cache is copied on gets.
	 */
	public void setCopyOnRead(Boolean copyOnRead) {
		this.copyOnRead = copyOnRead;
	}

	/**
	 * @return the copyOnRead
	 */
	public Boolean getCopyOnRead() {
		return copyOnRead;
	}

	/**
	 * Set the Cache's critical heap percentage attribute.
	 *
	 * @param criticalHeapPercentage floating point value indicating the critical heap percentage.
	 */
	public void setCriticalHeapPercentage(Float criticalHeapPercentage) {
		this.criticalHeapPercentage = criticalHeapPercentage;
	}

	/**
	 * @return the criticalHeapPercentage
	 */
	public Float getCriticalHeapPercentage() {
		return criticalHeapPercentage;
	}

	/**
	 * Set the cache's critical off-heap percentage property.
	 *
	 * @param criticalOffHeapPercentage floating point value indicating the critical off-heap percentage.
	 */
	public void setCriticalOffHeapPercentage(Float criticalOffHeapPercentage) {
		this.criticalOffHeapPercentage = criticalOffHeapPercentage;
	}

	/**
	 * @return the criticalOffHeapPercentage
	 */
	public Float getCriticalOffHeapPercentage() {
		return this.criticalOffHeapPercentage;
	}

	/**
	 * Sets an instance of the DynamicRegionSupport to support Dynamic Regions in this Pivotal GemFire Cache.
	 *
	 * @param dynamicRegionSupport the DynamicRegionSupport class to setup Dynamic Regions in this Cache.
	 */
	public void setDynamicRegionSupport(DynamicRegionSupport dynamicRegionSupport) {
		this.dynamicRegionSupport = dynamicRegionSupport;
	}

	/**
	 * @return the dynamicRegionSupport
	 */
	public DynamicRegionSupport getDynamicRegionSupport() {
		return dynamicRegionSupport;
	}

	/**
	 * Controls whether auto-reconnect functionality introduced in Pivotal GemFire 8 is enabled or not.
	 *
	 * @param enableAutoReconnect a boolean value to enable/disable auto-reconnect functionality.
	 * @since Pivotal GemFire 8.0
	 */
	public void setEnableAutoReconnect(Boolean enableAutoReconnect) {
		this.enableAutoReconnect = enableAutoReconnect;
	}

	/**
	 * Gets the value for the auto-reconnect setting.
	 *
	 * @return a boolean value indicating whether auto-reconnect was specified (non-null) and whether it was enabled
	 * or not.
	 */
	public Boolean getEnableAutoReconnect() {
		return this.enableAutoReconnect;
	}

	/**
	 * Set the Cache's eviction heap percentage attribute.
	 *
	 * @param evictionHeapPercentage float-point value indicating the Cache's heap use percentage to trigger eviction.
	 */
	public void setEvictionHeapPercentage(Float evictionHeapPercentage) {
		this.evictionHeapPercentage = evictionHeapPercentage;
	}

	/**
	 * @return the evictionHeapPercentage
	 */
	public Float getEvictionHeapPercentage() {
		return evictionHeapPercentage;
	}

	/**
	 * Set the cache's eviction off-heap percentage property.
	 *
	 * @param evictionOffHeapPercentage float-point value indicating the percentage of off-heap use triggering eviction.
	 */
	public void setEvictionOffHeapPercentage(Float evictionOffHeapPercentage) {
		this.evictionOffHeapPercentage = evictionOffHeapPercentage;
	}

	/**
	 * @return the evictionOffHeapPercentage
	 */
	public Float getEvictionOffHeapPercentage() {
		return this.evictionOffHeapPercentage;
	}

	/**
	 * Requires Pivotal GemFire 7.0 or higher
	 * @param gatewayConflictResolver defined as Object in the signature for backward
	 * compatibility with Gemfire 6 compatibility. This must be an instance of
	 * {@link org.apache.geode.cache.util.GatewayConflictResolver}
	 */
	public void setGatewayConflictResolver(GatewayConflictResolver gatewayConflictResolver) {
		this.gatewayConflictResolver = gatewayConflictResolver;
	}

	/**
	 * @return the gatewayConflictResolver
	 */
	public GatewayConflictResolver getGatewayConflictResolver() {
		return gatewayConflictResolver;
	}

	/**
	 * @param jndiDataSources the list of configured JndiDataSources to use with this Cache.
	 */
	public void setJndiDataSources(List<JndiDataSource> jndiDataSources) {
		this.jndiDataSources = jndiDataSources;
	}

	/**
	 * @return the list of configured JndiDataSources.
	 */
	public List<JndiDataSource> getJndiDataSources() {
		return jndiDataSources;
	}

	/**
	 * Sets the number of seconds for implicit and explicit object lock leases to timeout.
	 *
	 * @param lockLease an integer value indicating the object lock lease timeout.
	 */
	public void setLockLease(Integer lockLease) {
		this.lockLease = lockLease;
	}

	/**
	 * @return the lockLease
	 */
	public Integer getLockLease() {
		return lockLease;
	}

	/**
	 * Sets the number of seconds in which the implicit object lock request will timeout.
	 *
	 * @param lockTimeout an integer value specifying the object lock request timeout.
	 */
	public void setLockTimeout(Integer lockTimeout) {
		this.lockTimeout = lockTimeout;
	}

	/**
	 * @return the lockTimeout
	 */
	public Integer getLockTimeout() {
		return lockTimeout;
	}

	/**
	 * Set for client subscription queue synchronization when this member acts as a server to clients
	 * and server redundancy is used. Sets the frequency (in seconds) at which the primary server sends messages
	 * to its secondary servers to remove queued events that have already been processed by the clients.
	 *
	 * @param messageSyncInterval an integer value specifying the number of seconds in which the primary server
	 * sends messages to secondary servers.
	 */
	public void setMessageSyncInterval(Integer messageSyncInterval) {
		this.messageSyncInterval = messageSyncInterval;
	}

	/**
	 * @return the messageSyncInterval
	 */
	public Integer getMessageSyncInterval() {
		return messageSyncInterval;
	}

	/**
	 * Set the phase for the {@link Cache} bean in the lifecycle managed by the Spring container.
	 *
	 * @param phase {@link Integer#TYPE int} value indicating the phase of this {@link Cache} bean
	 * in the lifecycle managed by the Spring container.
	 * @see org.springframework.context.Phased#getPhase()
	 */
	protected void setPhase(int phase) {
		this.phase = phase;
	}

	/**
	 * Returns the configured phase of the {@link Cache} bean in the lifecycle managed by the Spring container.
	 *
	 * @return an {@link Integer#TYPE int} value indicating the phase of this {@link Cache} bean in the lifecycle
	 * managed by the Spring container.
	 * @see org.springframework.context.Phased#getPhase()
	 */
	@Override
	public int getPhase() {
		return this.phase;
	}

	/**
	 * Set the disk store that is used for PDX meta data. Applicable on GemFire
	 * 6.6 or higher.
	 *
	 * @param pdxDiskStoreName the pdxDiskStoreName to set
	 */
	public void setPdxDiskStoreName(String pdxDiskStoreName) {
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	/**
	 * @return the pdxDiskStoreName
	 */
	public String getPdxDiskStoreName() {
		return pdxDiskStoreName;
	}

	/**
	 * Controls whether pdx ignores fields that were unread during
	 * deserialization. Applicable on Pivotal GemFire 6.6 or higher.
	 *
	 * @param pdxIgnoreUnreadFields the pdxIgnoreUnreadFields to set
	 */
	public void setPdxIgnoreUnreadFields(Boolean pdxIgnoreUnreadFields) {
		this.pdxIgnoreUnreadFields = pdxIgnoreUnreadFields;
	}

	/**
	 * @return the pdxIgnoreUnreadFields
	 */
	public Boolean getPdxIgnoreUnreadFields() {
		return pdxIgnoreUnreadFields;
	}

	/**
	 * Controls whether type metadata for PDX objects is persisted to disk. Applicable on Pivotal GemFire 6.6 or higher.
	 *
	 * @param pdxPersistent a boolean value indicating that PDX type meta-data should be persisted to disk.
	 */
	public void setPdxPersistent(Boolean pdxPersistent) {
		this.pdxPersistent = pdxPersistent;
	}

	/**
	 * @return the pdxPersistent
	 */
	public Boolean getPdxPersistent() {
		return pdxPersistent;
	}

	/**
	 * Sets the object preference to PdxInstance. Applicable on Pivotal GemFire 6.6 or higher.
	 *
	 * @param pdxReadSerialized a boolean value indicating the PDX instance should be returned from Region.get(key)
	 * when available.
	 */
	public void setPdxReadSerialized(Boolean pdxReadSerialized) {
		this.pdxReadSerialized = pdxReadSerialized;
	}

	/**
	 * @return the pdxReadSerialized
	 */
	public Boolean getPdxReadSerialized() {
		return pdxReadSerialized;
	}

	/**
	 * Sets the {@link PdxSerializable} for this cache. Applicable on GemFire
	 * 6.6 or higher. The argument is of type object for compatibility with
	 * Pivotal GemFire 6.5.
	 *
	 * @param serializer pdx serializer configured for this cache.
	 */
	public void setPdxSerializer(PdxSerializer serializer) {
		this.pdxSerializer = serializer;
	}

	/**
	 * @return the pdxSerializer
	 */
	public PdxSerializer getPdxSerializer() {
		return pdxSerializer;
	}

	/**
	 * Null-safe operation to set an array of {@link PeerCacheConfigurer PeerCacheConfigurers} used to apply
	 * additional configuration to this {@link CacheFactoryBean} when using Annotation-based configuration.
	 *
	 * @param peerCacheConfigurers array of {@link PeerCacheConfigurer PeerCacheConfigurers} used to apply
	 * additional configuration to this {@link CacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 * @see #setPeerCacheConfigurers(List)
	 */
	public void setPeerCacheConfigurers(PeerCacheConfigurer... peerCacheConfigurers) {
		setPeerCacheConfigurers(Arrays.asList(nullSafeArray(peerCacheConfigurers, PeerCacheConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link PeerCacheConfigurer PeerCacheConfigurers} to apply
	 * additional configuration to this {@link CacheFactoryBean} when using Annotation-based configuration.
	 *
	 * @param peerCacheConfigurers {@link Iterable} of {@link PeerCacheConfigurer PeerCacheConfigurers} used to apply
	 * additional configuration to this {@link CacheFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
	 */
	public void setPeerCacheConfigurers(List<PeerCacheConfigurer> peerCacheConfigurers) {
		Optional.ofNullable(peerCacheConfigurers).ifPresent(this.peerCacheConfigurers::addAll);
	}

	/**
	 * Set the number of seconds a netSearch operation can wait for data before timing out.
	 *
	 * @param searchTimeout an integer value indicating the netSearch timeout value.
	 */
	public void setSearchTimeout(Integer searchTimeout) {
		this.searchTimeout = searchTimeout;
	}

	/**
	 * @return the searchTimeout
	 */
	public Integer getSearchTimeout() {
		return searchTimeout;
	}

	/**
	 * Configures the {@link org.apache.geode.security.SecurityManager} used to secure this cache.
	 *
	 * @param securityManager {@link org.apache.geode.security.SecurityManager} used to secure this cache.
	 * @see org.apache.geode.security.SecurityManager
	 */
	public void setSecurityManager(SecurityManager securityManager) {
		this.securityManager = securityManager;
	}

	/**
	 * Returns the {@link org.apache.geode.security.SecurityManager} used to secure this cache.
	 *
	 * @return the {@link org.apache.geode.security.SecurityManager} used to secure this cache.
	 * @see org.apache.geode.security.SecurityManager
	 */
	public SecurityManager getSecurityManager() {
		return securityManager;
	}

	/**
	 * Sets the list of TransactionListeners used to configure the Cache to receive transaction events after
	 * the transaction is processed (committed, rolled back).
	 *
	 * @param transactionListeners the list of Pivotal GemFire TransactionListeners listening for transaction events.
	 * @see org.apache.geode.cache.TransactionListener
	 */
	public void setTransactionListeners(List<TransactionListener> transactionListeners) {
		this.transactionListeners = transactionListeners;
	}

	/**
	 * @return the transactionListeners
	 */
	public List<TransactionListener> getTransactionListeners() {
		return transactionListeners;
	}

	/**
	 * Sets the TransactionWriter used to configure the Cache for handling transaction events, such as to veto
	 * the transaction or update an external DB before the commit.
	 *
	 * @param transactionWriter the Pivotal GemFire TransactionWriter callback receiving transaction events.
	 * @see org.apache.geode.cache.TransactionWriter
	 */
	public void setTransactionWriter(TransactionWriter transactionWriter) {
		this.transactionWriter = transactionWriter;
	}

	/**
	 * @return the transactionWriter
	 */
	public TransactionWriter getTransactionWriter() {
		return transactionWriter;
	}

	/**
	 * Sets whether to enable the {@link GemfireBeanFactoryLocator}.
	 *
	 * @param use boolean value indicating whether to enable the {@link GemfireBeanFactoryLocator}.
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
	 */
	public void setUseBeanFactoryLocator(boolean use) {
		this.useBeanFactoryLocator = use;
	}

	/**
	 * Determines whether the {@link GemfireBeanFactoryLocator} has been enabled.
	 *
	 * @return a boolean value indicating whether the {@link GemfireBeanFactoryLocator} has been enabled.
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
	 */
	public boolean isUseBeanFactoryLocator() {
		return this.useBeanFactoryLocator;
	}

	/**
	 * Sets the state of the {@literal use-shared-configuration} Pivotal GemFire/Apache Geode
	 * distribution configuration setting.
	 *
	 * @param useSharedConfiguration boolean value to set the {@literal use-shared-configuration}
	 * Pivotal GemFire/Apache Geode distribution configuration setting.
	 */
	public void setUseClusterConfiguration(Boolean useSharedConfiguration) {
		this.useClusterConfiguration = useSharedConfiguration;
	}

	/**
	 * Return the state of the {@literal use-shared-configuration} Pivotal GemFire/Apache Geode
	 * distribution configuration setting.
	 *
	 * @return the current boolean value for the {@literal use-shared-configuration}
	 * Pivotal GemFire/Apache Geode distribution configuration setting.
	 */
	public Boolean getUseClusterConfiguration() {
		return this.useClusterConfiguration;
	}

	/**
	 * Callback interface for initializing either a {@link CacheFactory} or a {@link ClientCacheFactory} instance,
	 * which is used to create an instance of {@link GemFireCache}.
	 *
	 * @see org.apache.geode.cache.CacheFactory
	 * @see org.apache.geode.cache.client.ClientCacheFactory
	 */
	@FunctionalInterface
	public interface CacheFactoryInitializer<T> {

		/**
		 * Initialize the given cache factory.
		 *
		 * @param cacheFactory cache factory to initialize.
		 * @return the given cache factory.
		 * @see org.apache.geode.cache.CacheFactory
		 * @see org.apache.geode.cache.client.ClientCacheFactory
		 */
		T initialize(T cacheFactory);

	}

	public static class DynamicRegionSupport {

		private Boolean persistent = Boolean.TRUE;
		private Boolean registerInterest = Boolean.TRUE;

		private String diskDirectory;
		private String poolName;

		public void setDiskDir(String diskDirectory) {
			this.diskDirectory = diskDirectory;
		}

		public String getDiskDir() {
			return diskDirectory;
		}

		public void setPersistent(Boolean persistent) {
			this.persistent = persistent;
		}

		public Boolean getPersistent() {
			return persistent;
		}

		public void setPoolName(String poolName) {
			this.poolName = poolName;
		}

		public String getPoolName() {
			return poolName;
		}

		public void setRegisterInterest(Boolean registerInterest) {
			this.registerInterest = registerInterest;
		}

		public Boolean getRegisterInterest() {
			return registerInterest;
		}

		public void initializeDynamicRegionFactory() {

			File localDiskDirectory = this.diskDirectory != null ? new File(this.diskDirectory) : null;

			DynamicRegionFactory.Config config =
				new DynamicRegionFactory.Config(localDiskDirectory, this.poolName, this.persistent,
					this.registerInterest);

			DynamicRegionFactory.get().open(config);
		}
	}

	public static class JndiDataSource {

		private List<ConfigProperty> configProperties;

		private Map<String, String> attributes;

		public Map<String, String> getAttributes() {
			return this.attributes;
		}

		public void setAttributes(Map<String, String> attributes) {
			this.attributes = attributes;
		}

		public List<ConfigProperty> getProps() {
			return this.configProperties;
		}

		public void setProps(List<ConfigProperty> props) {
			this.configProperties = props;
		}
	}
}
