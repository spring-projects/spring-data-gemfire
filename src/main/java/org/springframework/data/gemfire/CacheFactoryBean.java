/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.GemFireCheckedException;
import org.apache.geode.GemFireException;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.DynamicRegionFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.util.GatewayConflictResolver;
import org.apache.geode.distributed.DistributedMember;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.internal.datasource.ConfigProperty;
import org.apache.geode.internal.jndi.JNDIInvoker;
import org.apache.geode.pdx.PdxSerializable;
import org.apache.geode.pdx.PdxSerializer;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.Phased;
import org.springframework.core.io.Resource;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.support.PersistenceExceptionTranslator;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * FactoryBean used to configure a GemFire peer Cache node. Allows either retrieval of an existing, opened Cache
 * or the creation of a new Cache instance.
 * <p>
 * This class implements the {@link org.springframework.dao.support.PersistenceExceptionTranslator}
 * interface, as auto-detected by Spring's
 * {@link org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor}, for AOP-based translation
 * of native Exceptions to Spring DataAccessExceptions. Hence, the presence of this class automatically enables
 * a PersistenceExceptionTranslationPostProcessor to translate GemFire Exceptions appropriately.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.context.Phased
 * @see org.springframework.dao.support.PersistenceExceptionTranslator
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.CacheFactory
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.distributed.DistributedMember
 * @see org.apache.geode.distributed.DistributedSystem
 */
@SuppressWarnings("unused")
public class CacheFactoryBean implements BeanClassLoaderAware, BeanFactoryAware, BeanNameAware, FactoryBean<Cache>,
		Phased, InitializingBean, DisposableBean, PersistenceExceptionTranslator {

	private boolean close = true;
	private boolean useBeanFactoryLocator = false;

	private int phase = -1;

	protected final Log log = LogFactory.getLog(getClass());

	private BeanFactory beanFactory;

	private Boolean copyOnRead;
	private Boolean enableAutoReconnect;
	private Boolean pdxIgnoreUnreadFields;
	private Boolean pdxPersistent;
	private Boolean pdxReadSerialized;
	private Boolean useClusterConfiguration;

	private Cache cache;

	private ClassLoader beanClassLoader;

	private DynamicRegionSupport dynamicRegionSupport;

	private Float criticalHeapPercentage;
	private Float evictionHeapPercentage;

	protected GemfireBeanFactoryLocator beanFactoryLocator;

	private Integer lockLease;
	private Integer lockTimeout;
	private Integer messageSyncInterval;
	private Integer searchTimeout;

	private List<JndiDataSource> jndiDataSources;

	private List<TransactionListener> transactionListeners;

	// Declared with type 'Object' for backward compatibility
	private Object gatewayConflictResolver;
	private Object pdxSerializer;

	private Properties properties;

	private Resource cacheXml;

	private String beanName;
	private String cacheResolutionMessagePrefix;
	private String pdxDiskStoreName;

	private TransactionWriter transactionWriter;

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		postProcessBeforeCacheInitialization(resolveProperties());
	}

	/* (non-Javadoc) */
	protected void postProcessBeforeCacheInitialization(Properties gemfireProperties) {
		gemfireProperties.setProperty("disable-auto-reconnect", String.valueOf(
			!Boolean.TRUE.equals(getEnableAutoReconnect())));
		gemfireProperties.setProperty("use-cluster-configuration", String.valueOf(
			Boolean.TRUE.equals(getUseClusterConfiguration())));
	}

	/* (non-Javadoc) */
	protected void setCache(Cache cache) {
		this.cache = cache;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T getCache() {
		return (T) cache;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	@Override
	public Cache getObject() throws Exception {
		return (cache != null ? cache : init());
	}

	/* (non-Javadoc) */
	@SuppressWarnings("deprecation")
	Cache init() throws Exception {
		initBeanFactoryLocator();

		final ClassLoader currentThreadContextClassLoader = Thread.currentThread().getContextClassLoader();

		try {
			// use bean ClassLoader to load Spring configured, GemFire Declarable classes
			Thread.currentThread().setContextClassLoader(beanClassLoader);

			cache = postProcess(resolveCache());

			DistributedSystem system = cache.getDistributedSystem();

			DistributedMember member = system.getDistributedMember();

			log.info(String.format("Connected to Distributed System [%1$s] as Member [%2$s]"
				.concat("in Group(s) [%3$s] with Role(s) [%4$s] on Host [%5$s] having PID [%6$d]."),
					system.getName(), member.getId(), member.getGroups(), member.getRoles(), member.getHost(),
						member.getProcessId()));

			log.info(String.format("%1$s GemFire v.%2$s Cache [%3$s].", cacheResolutionMessagePrefix,
				CacheFactory.getVersion(), cache.getName()));

			return cache;
		}
		finally {
			Thread.currentThread().setContextClassLoader(currentThreadContextClassLoader);
		}
	}

	/* (non-Javadoc) */
	private void initBeanFactoryLocator() {
		if (useBeanFactoryLocator && beanFactoryLocator == null) {
			beanFactoryLocator = new GemfireBeanFactoryLocator();
			beanFactoryLocator.setBeanFactory(beanFactory);
			beanFactoryLocator.setBeanName(beanName);
			beanFactoryLocator.afterPropertiesSet();
		}
	}

	/**
	 * If Dynamic Regions are enabled, create and initialize a DynamicRegionFactory before creating the Cache.
	 */
	private void initDynamicRegionFactory() {
		if (dynamicRegionSupport != null) {
			dynamicRegionSupport.initializeDynamicRegionFactory();
		}
	}

	/**
	 * Resolves the GemFire Cache by first attempting to lookup and find an existing Cache instance in the VM;
	 * if an existing Cache could not be found, then this method proceeds in attempting to create a new Cache instance.
	 *
	 * @return the resolved GemFire Cache instance.
	 * @see org.apache.geode.cache.Cache
	 * @see #fetchCache()
	 * @see #createFactory(java.util.Properties)
	 * @see #prepareFactory(Object)
	 * @see #createCache(Object)
	 */
	protected Cache resolveCache() {
		try {
			cacheResolutionMessagePrefix = "Found existing";
			return (Cache) fetchCache();
		}
		catch (CacheClosedException ex) {
			cacheResolutionMessagePrefix = "Created new";
			initDynamicRegionFactory();
			return (Cache) createCache(prepareFactory(createFactory(resolveProperties())));
		}
	}

	/**
	 * Fetches an existing GemFire Cache instance from the CacheFactory.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @return the existing GemFire Cache instance if available.
	 * @throws org.apache.geode.cache.CacheClosedException if an existing GemFire Cache instance does not exist.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.CacheFactory#getAnyInstance()
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T fetchCache() {
		return (T) (cache != null ? cache : CacheFactory.getAnyInstance());
	}

	/**
	 * Resolves the GemFire System properties used to configure the GemFire Cache instance.
	 *
	 * @return a Properties object containing GemFire System properties used to configure the GemFire Cache instance.
	 * @see #getProperties()
	 */
	protected Properties resolveProperties() {
		return (properties != null ? properties : (properties = new Properties()));
	}

	/**
	 * Creates an instance of GemFire factory initialized with the given GemFire System Properties
	 * to create an instance of a GemFire cache.
	 *
	 * @param gemfireProperties a Properties object containing GemFire System properties.
	 * @return an instance of a GemFire factory used to create a GemFire cache instance.
	 * @see java.util.Properties
	 * @see org.apache.geode.cache.CacheFactory
	 */
	protected Object createFactory(Properties gemfireProperties) {
		return new CacheFactory(gemfireProperties);
	}

	/**
	 * Initializes the GemFire factory used to create the GemFire cache instance.  Sets PDX options
	 * specified by the user.
	 *
	 * @param factory the GemFire factory used to create an instance of the GemFire cache.
	 * @return the initialized GemFire cache factory.
	 * @see #isPdxOptionsSpecified()
	 */
	protected Object prepareFactory(Object factory) {
		return initializePdx((CacheFactory) factory);
	}

	/**
	 * Initialize the PDX settings on the {@link CacheFactory}.
	 *
	 * @param cacheFactory the GemFire {@link CacheFactory} used to configure and create a GemFire {@link Cache}.
	 * @see org.apache.geode.cache.CacheFactory
	 */
	CacheFactory initializePdx(CacheFactory cacheFactory) {
		if (isPdxOptionsSpecified()) {
			if (pdxSerializer != null) {
				Assert.isInstanceOf(PdxSerializer.class, pdxSerializer,
					String.format("[%1$s] of type [%2$s] is not a PdxSerializer", pdxSerializer,
						ObjectUtils.nullSafeClassName(pdxSerializer)));

				cacheFactory.setPdxSerializer((PdxSerializer) pdxSerializer);
			}
			if (pdxDiskStoreName != null) {
				cacheFactory.setPdxDiskStore(pdxDiskStoreName);
			}
			if (pdxIgnoreUnreadFields != null) {
				cacheFactory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			}
			if (pdxPersistent != null) {
				cacheFactory.setPdxPersistent(pdxPersistent);
			}
			if (pdxReadSerialized != null) {
				cacheFactory.setPdxReadSerialized(pdxReadSerialized);
			}
		}

		return cacheFactory;
	}

	/**
	 * Determines whether the user specified PDX options.
	 *
	 * @return a boolean value indicating whether the user specified PDX options or not.
	 */
	protected boolean isPdxOptionsSpecified() {
		return (pdxSerializer != null || pdxReadSerialized != null || pdxPersistent != null
			|| pdxIgnoreUnreadFields != null || pdxDiskStoreName != null);
	}

	/**
	 * Creates a new GemFire cache instance using the provided factory.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @param factory the appropriate GemFire factory used to create a cache instance.
	 * @return an instance of the GemFire cache.
	 * @see org.apache.geode.cache.GemFireCache
	 * @see org.apache.geode.cache.CacheFactory#create()
	 */
	@SuppressWarnings("unchecked")
	protected <T extends GemFireCache> T createCache(Object factory) {
		return (T) (cache != null ? cache : ((CacheFactory) factory).create());
	}

	/**
	 * Post processes the GemFire Cache instance by loading any cache.xml, applying settings specified in SDG XML
	 * configuration meta-data, and registering the appropriate Transaction Listeners, Writer and JNDI settings.
	 *
	 * @param <T> parameterized Class type extension of GemFireCache.
	 * @param cache the GemFire Cache instance to process.
	 * @return the GemFire Cache instance after processing.
	 * @throws IOException if the cache.xml Resource could not be loaded and applied to the Cache instance.
	 * @see org.apache.geode.cache.Cache#loadCacheXml(java.io.InputStream)
	 * @see #getCacheXml()
	 * @see #setHeapPercentages(org.apache.geode.cache.GemFireCache)
	 * @see #registerTransactionListeners(org.apache.geode.cache.GemFireCache)
	 * @see #registerTransactionWriter(org.apache.geode.cache.GemFireCache)
	 * @see #registerJndiDataSources()
	 */
	protected <T extends GemFireCache> T postProcess(T cache) throws IOException {
		Resource localCacheXml = getCacheXml();

		// load cache.xml Resource and initialize the Cache
		if (localCacheXml != null) {
			if (log.isDebugEnabled()) {
				log.debug(String.format("initializing Cache with '%1$s'", cacheXml));
			}

			cache.loadCacheXml(localCacheXml.getInputStream());
		}

		if (this.copyOnRead != null) {
			cache.setCopyOnRead(this.copyOnRead);
		}
		if (gatewayConflictResolver != null) {
			((Cache) cache).setGatewayConflictResolver((GatewayConflictResolver) gatewayConflictResolver);
		}
		if (lockLease != null) {
			((Cache) cache).setLockLease(lockLease);
		}
		if (lockTimeout != null) {
			((Cache) cache).setLockTimeout(lockTimeout);
		}
		if (messageSyncInterval != null) {
			((Cache) cache).setMessageSyncInterval(messageSyncInterval);
		}
		if (searchTimeout != null) {
			((Cache) cache).setSearchTimeout(searchTimeout);
		}

		setHeapPercentages(cache);
		registerTransactionListeners(cache);
		registerTransactionWriter(cache);
		registerJndiDataSources();

		return cache;
	}

	/* (non-Javadoc) */
	private void setHeapPercentages(GemFireCache cache) {
		if (criticalHeapPercentage != null) {
			Assert.isTrue(criticalHeapPercentage > 0.0 && criticalHeapPercentage <= 100.0,
				String.format("'criticalHeapPercentage' (%1$s) is invalid; must be > 0.0 and <= 100.0",
					criticalHeapPercentage));
			cache.getResourceManager().setCriticalHeapPercentage(criticalHeapPercentage);
		}

		if (evictionHeapPercentage != null) {
			Assert.isTrue(evictionHeapPercentage > 0.0 && evictionHeapPercentage <= 100.0,
				String.format("'evictionHeapPercentage' (%1$s) is invalid; must be > 0.0 and <= 100.0",
					evictionHeapPercentage));
			cache.getResourceManager().setEvictionHeapPercentage(evictionHeapPercentage);
		}
	}

	/* (non-Javadoc) */
	private void registerTransactionListeners(GemFireCache cache) {
		for (TransactionListener transactionListener : CollectionUtils.nullSafeCollection(transactionListeners)) {
			cache.getCacheTransactionManager().addListener(transactionListener);
		}
	}

	/* (non-Javadoc) */
	private void registerTransactionWriter(GemFireCache cache) {
		if (transactionWriter != null) {
			cache.getCacheTransactionManager().setWriter(transactionWriter);
		}
	}

	/* (non-Javadoc) */
	private void registerJndiDataSources() {
		for (JndiDataSource jndiDataSource : CollectionUtils.nullSafeCollection(jndiDataSources)) {
			String typeAttributeValue = jndiDataSource.getAttributes().get("type");
			JndiDataSourceType jndiDataSourceType = JndiDataSourceType.valueOfIgnoreCase(typeAttributeValue);

			Assert.notNull(jndiDataSourceType, String.format(
				"'jndi-binding' 'type' [%1$s] is invalid; 'type' must be one of %2$s", typeAttributeValue,
					Arrays.toString(JndiDataSourceType.values())));

			jndiDataSource.getAttributes().put("type", jndiDataSourceType.getName());
			JNDIInvoker.mapDatasource(jndiDataSource.getAttributes(), jndiDataSource.getProps());
		}
	}

	@Override
	public void destroy() throws Exception {
		if (close) {
			Cache localCache = fetchCache();

			if (localCache != null && !localCache.isClosed()) {
				close(localCache);
			}

			this.cache = null;

			if (beanFactoryLocator != null) {
				beanFactoryLocator.destroy();
				beanFactoryLocator = null;
			}
		}
	}

	/* (non-Javadoc) */
	protected void close(GemFireCache cache) {
		cache.close();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#getObjectType()
	 */
	@Override
	public Class<? extends GemFireCache> getObjectType() {
		return (cache != null ? cache.getClass() : Cache.class);
	}

	/* (non-Javadoc) */
	protected void setPhase(int phase) {
		this.phase = phase;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.context.Phased#getPhase()
	 */
	@Override
	public int getPhase() {
		return phase;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}

	@Override
	public DataAccessException translateExceptionIfPossible(RuntimeException e) {
		if (e instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) e);
		}

		if (e instanceof IllegalArgumentException) {
			DataAccessException wrapped = GemfireCacheUtils.convertQueryExceptions(e);
			// ignore conversion if the generic exception is returned
			if (!(wrapped instanceof GemfireSystemException)) {
				return wrapped;
			}
		}

		if (e.getCause() instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) e.getCause());
		}

		if (e.getCause() instanceof GemFireCheckedException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireCheckedException) e.getCause());
		}

		return null;
	}

	/**
	 * Sets a reference to the {@link ClassLoader} used to load and create bean classes in the Spring container.
	 *
	 * @param classLoader the {@link ClassLoader} used to load and create beans in the Spring container.
	 * @see java.lang.ClassLoader
	 */
	@Override
	public void setBeanClassLoader(ClassLoader classLoader) {
		this.beanClassLoader = classLoader;
	}

	/**
	 * Gets a reference to the {@link ClassLoader} used to load and create bean classes in the Spring container.
	 *
	 * @return the {@link ClassLoader} used to load and create beans in the Spring container.
	 * @see java.lang.ClassLoader
	 */
	public ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	/**
	 * Sets a reference to the Spring {@link BeanFactory} containing this GemFire {@link Cache} {@link FactoryBean}.
	 *
	 * @param beanFactory a reference to the Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #getBeanFactory()
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Gets a reference to the Spring BeanFactory containing this GemFire {@link Cache} {@link FactoryBean}.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #setBeanFactory(BeanFactory)
	 */
	public BeanFactory getBeanFactory() {
		return beanFactory;
	}

	/* (non-Javadoc) */
	public GemfireBeanFactoryLocator getBeanFactoryLocator() {
		return beanFactoryLocator;
	}

	/**
	 * Sets the Spring bean name for this GemFire {@link Cache}.
	 *
	 * @param name a String value indicating the Spring container bean name for the GemFire {@link Cache} object.
	 */
	@Override
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Gets the Spring bean name for this GemFire {@link Cache}.
	 *
	 * @return a String value indicating the Spring container bean name for the GemFire {@link Cache} object.
	 */
	public String getBeanName() {
		return beanName;
	}

	/**
	 * Sets the {@link Cache} configuration meta-data.
	 *
	 * @param cacheXml the cache.xml {@link Resource} used to initialize the GemFire {@link Cache}.
	 * @see org.springframework.core.io.Resource
	 */
	public void setCacheXml(Resource cacheXml) {
		this.cacheXml = cacheXml;
	}

	/**
	 * Gets a reference to the GemFire native cache.xml file as a Spring {@link Resource}.
	 *
	 * @return the a reference to the GemFire native cache.xml as a Spring {@link Resource}.
	 * @see org.springframework.core.io.Resource
	 */
	public Resource getCacheXml() {
		return cacheXml;
	}

	/* (non-Javadoc) */
	private File getCacheXmlFile() {
		try {
			return getCacheXml().getFile();
		}
		catch (IOException e) {
			throw new IllegalStateException(String.format("Resource (%1$s) is not resolvable as a file", e));
		}
	}

	/* (non-Javadoc) */
	private boolean isCacheXmlAvailable() {
		try {
			Resource localCacheXml = getCacheXml();
			return (localCacheXml != null && localCacheXml.getFile().isFile());
		}
		catch (IOException ignore) {
			return false;
		}
	}

	/**
	 * Sets the cache properties.
	 *
	 * @param properties the properties to set
	 */
	public void setProperties(Properties properties) {
		this.properties = properties;
	}

	/**
	 * Gets a reference to the GemFire System Properties.
	 *
	 * @return a reference to the GemFire System Properties.
	 * @see java.util.Properties
	 */
	public Properties getProperties() {
		return properties;
	}

	/**
	 * Set whether the Cache should be closed.
	 *
	 * @param close set to false if destroy() should not close the cache
	 */
	public void setClose(boolean close) {
		this.close = close;
	}

	/**
	 * @return close.
	 */
	public Boolean getClose() {
		return close;
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
	 * Sets an instance of the DynamicRegionSupport to support Dynamic Regions in this GemFire Cache.
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
	 * Controls whether auto-reconnect functionality introduced in GemFire 8 is enabled or not.
	 *
	 * @param enableAutoReconnect a boolean value to enable/disable auto-reconnect functionality.
	 * @since GemFire 8.0
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
		return enableAutoReconnect;
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
	 * Requires GemFire 7.0 or higher
	 * @param gatewayConflictResolver defined as Object in the signature for backward
	 * compatibility with Gemfire 6 compatibility. This must be an instance of
	 * {@link org.apache.geode.cache.util.GatewayConflictResolver}
	 */
	public void setGatewayConflictResolver(Object gatewayConflictResolver) {
		this.gatewayConflictResolver = gatewayConflictResolver;
	}

	/**
	 * @return the gatewayConflictResolver
	 */
	public Object getGatewayConflictResolver() {
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
	 * deserialization. Applicable on GemFire 6.6 or higher.
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
	 * Controls whether type metadata for PDX objects is persisted to disk. Applicable on GemFire 6.6 or higher.
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
	 * Sets the object preference to PdxInstance. Applicable on GemFire 6.6 or higher.
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
	 * GemFire 6.5.
	 *
	 * @param serializer pdx serializer configured for this cache.
	 */
	public void setPdxSerializer(Object serializer) {
		this.pdxSerializer = serializer;
	}

	/**
	 * @return the pdxSerializer
	 */
	public Object getPdxSerializer() {
		return pdxSerializer;
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
	 * Sets the list of TransactionListeners used to configure the Cache to receive transaction events after
	 * the transaction is processed (committed, rolled back).
	 *
	 * @param transactionListeners the list of GemFire TransactionListeners listening for transaction events.
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
	 * @param transactionWriter the GemFire TransactionWriter callback receiving transaction events.
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
	 * Indicates whether a bean factory locator is enabled for this cache definition or not. The locator stores
	 * the enclosing bean factory reference to allow auto-wiring of Spring beans into GemFire managed classes.
	 * Usually disabled when the same cache is used in multiple application context/bean factories inside
	 * the same VM.
	 *
	 * @param usage true if the bean factory locator is to be used underneath the hood.
	 */
	public void setUseBeanFactoryLocator(boolean usage) {
		this.useBeanFactoryLocator = usage;
	}

	/**
	 * @return useBeanFactoryLocator
	 */
	public boolean isUseBeanFactoryLocator() {
		return useBeanFactoryLocator;
	}

	/**
	 * Sets the state of the use-shared-configuration GemFire distribution config setting.
	 *
	 * @param useSharedConfiguration a boolean value to set the use-shared-configuration GemFire distribution property.
	 */
	public void setUseClusterConfiguration(Boolean useSharedConfiguration) {
		this.useClusterConfiguration = useSharedConfiguration;
	}

	/**
	 * Gets the value of the use-shared-configuration GemFire configuration setting.
	 *
	 * @return a boolean value indicating whether shared configuration use has been enabled or not.
	 */
	public Boolean getUseClusterConfiguration() {
		return this.useClusterConfiguration;
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
			File localDiskDirectory = (this.diskDirectory != null ? new File(this.diskDirectory) : null);

			DynamicRegionFactory.Config config = new DynamicRegionFactory.Config(localDiskDirectory, poolName,
				persistent, registerInterest);

			DynamicRegionFactory.get().open(config);
		}
	}

	public static class JndiDataSource {

		private List<ConfigProperty> props;
		private Map<String, String> attributes;

		public Map<String, String> getAttributes() {
			return attributes;
		}

		public void setAttributes(Map<String, String> attributes) {
			this.attributes = attributes;
		}

		public List<ConfigProperty> getProps() {
			return props;
		}

		public void setProps(List<ConfigProperty> props) {
			this.props = props;
		}
	}

}
