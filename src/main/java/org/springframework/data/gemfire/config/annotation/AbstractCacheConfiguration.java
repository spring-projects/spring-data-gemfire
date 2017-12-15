/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.springframework.data.gemfire.CacheFactoryBean.DynamicRegionSupport;
import static org.springframework.data.gemfire.CacheFactoryBean.JndiDataSource;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeList;

import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.util.GatewayConflictResolver;
import org.apache.geode.pdx.PdxSerializer;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.io.Resource;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.support.CustomEditorBeanFactoryPostProcessor;
import org.springframework.data.gemfire.config.support.DefinedIndexesApplicationListener;
import org.springframework.data.gemfire.config.support.DiskStoreDirectoryBeanPostProcessor;
import org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.MappingPdxSerializer;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.util.StringUtils;

/**
 * {@link AbstractCacheConfiguration} is an abstract base class for configuring either a Pivotal GemFire/Apache Geode
 * client or peer-based cache instance using Spring's Java-based, Annotation {@link Configuration} support.
 *
 * This class encapsulates configuration settings common to both Pivotal GemFire/Apache Geode
 * {@link org.apache.geode.cache.Cache peer caches}
 * and {@link org.apache.geode.cache.client.ClientCache client caches}.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see java.util.Properties
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.convert.ConversionService
 * @see org.springframework.core.io.Resource
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.config.support.CustomEditorBeanFactoryPostProcessor
 * @see org.springframework.data.gemfire.config.support.DefinedIndexesApplicationListener
 * @see org.springframework.data.gemfire.config.support.DiskStoreDirectoryBeanPostProcessor
 * @see org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor
 * @see org.springframework.data.gemfire.mapping.GemfireMappingContext
 * @see org.springframework.data.gemfire.mapping.MappingPdxSerializer
 * @since 1.9.0
 */
@Configuration
@SuppressWarnings("unused")
public abstract class AbstractCacheConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	private static final AtomicBoolean CUSTOM_EDITORS_REGISTERED = new AtomicBoolean(false);
	private static final AtomicBoolean DEFINED_INDEXES_APPLICATION_LISTENER_REGISTERED = new AtomicBoolean(false);
	private static final AtomicBoolean DISK_STORE_DIRECTORY_BEAN_POST_PROCESSOR_REGISTERED = new AtomicBoolean(false);
	private static final AtomicBoolean PDX_DISK_STORE_AWARE_BEAN_FACTORY_POST_PROCESSOR_REGISTERED = new AtomicBoolean(false);

	protected static final boolean DEFAULT_CLOSE = true;
	protected static final boolean DEFAULT_COPY_ON_READ = false;
	protected static final boolean DEFAULT_USE_BEAN_FACTORY_LOCATOR = false;

	protected static final int DEFAULT_MCAST_PORT = 0;

	protected static final String DEFAULT_LOCATORS = "";
	protected static final String DEFAULT_LOG_LEVEL = "config";
	protected static final String DEFAULT_NAME = "SpringDataGemFireApplication";

	private boolean close = DEFAULT_CLOSE;
	private boolean copyOnRead = DEFAULT_COPY_ON_READ;
	private boolean useBeanFactoryLocator = DEFAULT_USE_BEAN_FACTORY_LOCATOR;

	private Boolean pdxIgnoreUnreadFields;
	private Boolean pdxPersistent;
	private Boolean pdxReadSerialized;

	private DynamicRegionSupport dynamicRegionSupport;

	private Integer mcastPort = 0;

	private Float criticalHeapPercentage;
	private Float evictionHeapPercentage;

	private GatewayConflictResolver gatewayConflictResolver;

	@Autowired(required = false)
	private GemfireMappingContext mappingContext;

	private List<JndiDataSource> jndiDataSources;
	private List<TransactionListener> transactionListeners;

	private PdxSerializer pdxSerializer;

	private PropertiesBuilder customGemFireProperties = PropertiesBuilder.create();

	private Resource cacheXml;

	private String locators = DEFAULT_LOCATORS;
	private String logLevel = DEFAULT_LOG_LEVEL;
	private String name;
	private String pdxDiskStoreName;
	private String startLocator;

	private TransactionWriter transactionWriter;

	/**
	 * Returns a {@link Properties} object containing Pivotal GemFire/Apache Geode properties used to configure
	 * the Pivotal GemFire/Apache Geode cache.
	 *
	 * The {@literal name} of the Pivotal GemFire/Apache Geode member/node in the cluster is set to a default,
	 * pre-defined and descriptive value depending on the type of configuration meta-data applied.
	 *
	 * {@literal mcast-port} is set to {@literal 0} and {@literal locators} is set to an {@link String empty String},
	 * which is necessary for {@link ClientCache cache client}-based applications.  These values can be changed
	 * and set accoridingly for {@link Cache peer cache} and {@link CacheServer cache server} applications.
	 *
	 * Finally, the {@literal log-level} property defaults to {@literal config}.
	 *
	 * @return a {@link Properties} object containing Pivotal GemFire/Apache Geode properties used to configure
	 * the Pivotal GemFire/Apache Geode cache instance.
	 * @see <a link="http://gemfire.docs.pivotal.io/docs-gemfire/reference/topics/gemfire_properties.html">GemFire Properties</a>
	 * @see java.util.Properties
	 * @see #locators()
	 * @see #logLevel()
	 * @see #mcastPort()
	 * @see #name()
	 */
	@Bean
	protected Properties gemfireProperties() {

		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty("name", name());
		gemfireProperties.setProperty("mcast-port", mcastPort());
		gemfireProperties.setProperty("log-level", logLevel());
		gemfireProperties.setProperty("locators", locators());
		gemfireProperties.setProperty("start-locator", startLocator());
		gemfireProperties.add(this.customGemFireProperties);

		return gemfireProperties.build();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {
		configureInfrastructure(importMetadata);
		configureCache(importMetadata);
		configurePdx(importMetadata);
		configureTheRest(importMetadata);
	}

	/**
	 * Configures Spring container infrastructure components and beans used by Spring Data GemFire
	 * to enable Pivotal GemFire or Apache Geode to function properly inside a Spring context.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing annotation meta-data
	 * for the Spring Data GemFire cache application class.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	protected void configureInfrastructure(AnnotationMetadata importMetadata) {
		registerCustomEditorBeanFactoryPostProcessor(importMetadata);
		registerDefinedIndexesApplicationListener(importMetadata);
		registerDiskStoreDirectoryBeanPostProcessor(importMetadata);
	}

	/* (non-Javadoc) */
	private void registerCustomEditorBeanFactoryPostProcessor(AnnotationMetadata importMetadata) {

		if (CUSTOM_EDITORS_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(CustomEditorBeanFactoryPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/* (non-Javadoc) */
	private void registerDefinedIndexesApplicationListener(AnnotationMetadata importMetadata) {

		if (DEFINED_INDEXES_APPLICATION_LISTENER_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(DefinedIndexesApplicationListener.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/* (non-Javadoc) */
	private void registerDiskStoreDirectoryBeanPostProcessor(AnnotationMetadata importMetadata) {

		if (DISK_STORE_DIRECTORY_BEAN_POST_PROCESSOR_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(DiskStoreDirectoryBeanPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/**
	 * Configures Pivotal GemFire/Apache Geode cache specific settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing the cache meta-data used to configure the cache.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	protected void configureCache(AnnotationMetadata importMetadata) {

		if (isClientPeerOrServerCacheApplication(importMetadata)) {

			Map<String, Object> cacheMetadataAttributes =
				importMetadata.getAnnotationAttributes(getAnnotationTypeName());

			setCopyOnRead(resolveProperty(cacheProperty("copy-on-read"),
				Boolean.TRUE.equals(cacheMetadataAttributes.get("copyOnRead"))));

			Optional.ofNullable(resolveProperty(cacheProperty("critical-heap-percentage"), (Float) null))
				.ifPresent(this::setCriticalHeapPercentage);

			Optional.ofNullable((Float) cacheMetadataAttributes.get("criticalHeapPercentage"))
				.filter(it -> getCriticalHeapPercentage() == null)
				.filter(AbstractAnnotationConfigSupport::hasValue)
				.ifPresent(this::setCriticalHeapPercentage);

			Optional.ofNullable(resolveProperty(cacheProperty("eviction-heap-percentage"), (Float) null))
				.ifPresent(this::setEvictionHeapPercentage);

			Optional.ofNullable((Float) cacheMetadataAttributes.get("evictionHeapPercentage"))
				.filter(it -> getEvictionHeapPercentage() == null)
				.filter(AbstractAnnotationConfigSupport::hasValue)
				.ifPresent(this::setEvictionHeapPercentage);

			setLogLevel(resolveProperty(cacheProperty("log-level"),
				(String) cacheMetadataAttributes.get("logLevel")));

			setName(resolveProperty(propertyName("name"),
				resolveProperty(cacheProperty("name"),
					(String) cacheMetadataAttributes.get("name"))));

			setUseBeanFactoryLocator(resolveProperty(propertyName("use-bean-factory-locator"),
				Boolean.TRUE.equals(cacheMetadataAttributes.get("useBeanFactoryLocator"))));
		}
	}

	/**
	 * Configures Pivotal GemFire/Apache Geode cache PDX Serialization.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing PDX meta-data used to configure the cache
	 * with PDX de/serialization capabilities.
	 * @see org.springframework.core.type.AnnotationMetadata
	 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/developing/data_serialization/gemfire_pdx_serialization.html">GemFire PDX Serialization</a>
	 */
	protected void configurePdx(AnnotationMetadata importMetadata) {

		String enablePdxTypeName = EnablePdx.class.getName();

		if (importMetadata.hasAnnotation(enablePdxTypeName)) {

			Map<String, Object> enablePdxAttributes = importMetadata.getAnnotationAttributes(enablePdxTypeName);

			setPdxDiskStoreName(resolveProperty(pdxProperty("disk-store-name"),
				(String) enablePdxAttributes.get("diskStoreName")));

			setPdxIgnoreUnreadFields(resolveProperty(pdxProperty("ignore-unread-fields"),
				Boolean.TRUE.equals(enablePdxAttributes.get("ignoreUnreadFields"))));

			setPdxPersistent(resolveProperty(pdxProperty("persistent"),
				Boolean.TRUE.equals(enablePdxAttributes.get("persistent"))));

			setPdxReadSerialized(resolveProperty(pdxProperty("read-serialized"),
				Boolean.TRUE.equals(enablePdxAttributes.get("readSerialized"))));

			setPdxSerializer(resolvePdxSerializer((String) enablePdxAttributes.get("serializerBeanName")));

			registerPdxDiskStoreAwareBeanFactoryPostProcessor(importMetadata);
		}
	}

	/**
	 * Resolves the {@link PdxSerializer} used to configure the cache for PDX De/Serialization.
	 *
	 * @param pdxSerializerBeanName {@link String} containing the name of a Spring bean
	 * implementing the {@link PdxSerializer} interface.
	 * @return the resolved {@link PdxSerializer} from configuration.
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see #newPdxSerializer(BeanFactory)
	 * @see #getBeanFactory()
	 */
	protected PdxSerializer resolvePdxSerializer(String pdxSerializerBeanName) {

		BeanFactory beanFactory = getBeanFactory();

		return Optional.ofNullable(pdxSerializerBeanName)
			.filter(beanFactory::containsBean)
			.map(beanName -> beanFactory.getBean(beanName, PdxSerializer.class))
			.orElseGet(() -> Optional.ofNullable(getPdxSerializer()).orElseGet(() -> newPdxSerializer(beanFactory)));
	}

	/**
	 * Constructs a new instance of {@link PdxSerializer}.
	 *
	 * @param <T> {@link Class} type of the {@link PdxSerializer}.
	 * @return a new instance of {@link PdxSerializer}.
	 * @see org.apache.geode.pdx.PdxSerializer
	 * @see #newPdxSerializer(BeanFactory)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends PdxSerializer> T newPdxSerializer() {
		return newPdxSerializer(getBeanFactory());
	}

	/**
	 * Constructs a new instance of {@link MappingPdxSerializer}.
	 *
	 * @param <T> {@link Class} type of the {@link PdxSerializer}; this method returns a {@link MappingPdxSerializer}.
	 * @param beanFactory {@link BeanFactory} used to get an instance of {@link ConversionService}
	 * used by the {@link MappingPdxSerializer}.
	 * @return a new instance of {@link MappingPdxSerializer}.
	 * @see org.springframework.data.gemfire.mapping.MappingPdxSerializer
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see org.apache.geode.pdx.PdxSerializer
	 */
	@SuppressWarnings("unchecked")
	protected <T extends PdxSerializer> T newPdxSerializer(BeanFactory beanFactory) {

		Optional<ConversionService> conversionService = Optional.ofNullable(beanFactory)
			.filter(it -> it instanceof ConfigurableBeanFactory)
			.map(it -> ((ConfigurableBeanFactory) it).getConversionService());

		return (T) MappingPdxSerializer.create(this.mappingContext, conversionService.orElse(null));
	}

	/* (non-Javadoc) */
	private void registerPdxDiskStoreAwareBeanFactoryPostProcessor(AnnotationMetadata importMetadata) {

		Optional.ofNullable(getPdxDiskStoreName())
			.filter(StringUtils::hasText)
			.ifPresent(pdxDiskStoreName -> {
				if (PDX_DISK_STORE_AWARE_BEAN_FACTORY_POST_PROCESSOR_REGISTERED.compareAndSet(false, true)) {
					register(BeanDefinitionBuilder.rootBeanDefinition(PdxDiskStoreAwareBeanFactoryPostProcessor.class)
						.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
						.addConstructorArgValue(pdxDiskStoreName)
						.getBeanDefinition());
				}
			});
	}

	/**
	 * Callback method allowing developers to configure other cache or application specific configuration settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing meta-data used to configure the cache or application.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	protected void configureTheRest(AnnotationMetadata importMetadata) {
	}

	/**
	 * Constructs a new, initialized instance of {@link CacheFactoryBean} based on the Spring application's
	 * cache type preference (i.e. client or peer), which is expressed via the appropriate annotation.
	 *
	 * Use the {@link ClientCacheApplication} Annotation to construct a {@link ClientCache cache client} application.
	 *
	 * Use the {@link PeerCacheApplication} Annotation to construct a {@link Cache peer cache} application.
	 *
	 * @param <T> {@link Class} specific sub-type of the {@link CacheFactoryBean}.
	 * @return a new instance of the appropriate {@link CacheFactoryBean} given the Spring application's
	 * cache type preference (i.e client or peer),  (e.g. {@link ClientCacheApplication}
	 * or {@link PeerCacheApplication}).
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 * @see #configureCacheFactoryBean(CacheFactoryBean)
	 * @see #newCacheFactoryBean()
	 */
	protected <T extends CacheFactoryBean> T constructCacheFactoryBean() {
		return configureCacheFactoryBean(this.<T>newCacheFactoryBean());
	}

	/**
	 * Constructs a new, uninitialized instance of {@link CacheFactoryBean} based on the Spring application's
	 * cache type preference (i.e. client or peer), which is expressed via the appropriate annotation.
	 *
	 * Use the {@link ClientCacheApplication} Annotation to construct a {@link ClientCache cache client} application.
	 *
	 * Use the {@link PeerCacheApplication} Annotation to construct a {@link Cache peer cache} application.
	 *
	 * @param <T> {@link Class} specific sub-type of the {@link CacheFactoryBean}.
	 * @return a new instance of the appropriate {@link CacheFactoryBean} given the Spring application's
	 * cache type preference (i.e client or peer),  (e.g. {@link ClientCacheApplication}
	 * or {@link PeerCacheApplication}).
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 */
	protected abstract <T extends CacheFactoryBean> T newCacheFactoryBean();

	/**
	 * Configures the {@link CacheFactoryBean} with common cache configuration settings.
	 *
	 * @param <T> {@link Class} specific sub-type of the {@link CacheFactoryBean}.
	 * @param gemfireCache {@link CacheFactoryBean} to configure.
	 * @return the given {@link CacheFactoryBean} with common cache configuration settings applied.
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 */
	protected <T extends CacheFactoryBean> T configureCacheFactoryBean(T gemfireCache) {

		gemfireCache.setBeanClassLoader(getBeanClassLoader());
		gemfireCache.setBeanFactory(getBeanFactory());
		gemfireCache.setCacheXml(getCacheXml());
		gemfireCache.setClose(isClose());
		gemfireCache.setCopyOnRead(getCopyOnRead());
		gemfireCache.setCriticalHeapPercentage(getCriticalHeapPercentage());
		gemfireCache.setDynamicRegionSupport(getDynamicRegionSupport());
		gemfireCache.setEvictionHeapPercentage(getEvictionHeapPercentage());
		gemfireCache.setGatewayConflictResolver(getGatewayConflictResolver());
		gemfireCache.setJndiDataSources(getJndiDataSources());
		gemfireCache.setProperties(gemfireProperties());
		gemfireCache.setPdxDiskStoreName(getPdxDiskStoreName());
		gemfireCache.setPdxIgnoreUnreadFields(getPdxIgnoreUnreadFields());
		gemfireCache.setPdxPersistent(getPdxPersistent());
		gemfireCache.setPdxReadSerialized(getPdxReadSerialized());
		gemfireCache.setPdxSerializer(getPdxSerializer());
		gemfireCache.setTransactionListeners(getTransactionListeners());
		gemfireCache.setTransactionWriter(getTransactionWriter());

		return gemfireCache;
	}

	/**
	 * Returns the cache application {@link java.lang.annotation.Annotation} type pertaining to this configuration.
	 *
	 * @return the cache application {@link java.lang.annotation.Annotation} type used by this application.
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerApplication
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
	 */
	protected abstract Class<? extends Annotation> getAnnotationType();

	/**
	 * Returns the fully-qualified {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 *
	 * @return the fully-qualified {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 * @see java.lang.Class#getName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/**
	 * Returns the simple {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 *
	 * @return the simple {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 * @see java.lang.Class#getSimpleName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeSimpleName() {
		return getAnnotationType().getSimpleName();
	}

	// REVIEW JAVADOC FROM HERE

	/**
	 * Determines whether this is a GemFire {@link org.apache.geode.cache.server.CacheServer} application,
	 * which is indicated by the presence of the {@link CacheServerApplication} annotation on a Spring application
	 * {@link org.springframework.context.annotation.Configuration @Configuration} class.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing application configuration meta-data
	 * from the annotations used to configure the Spring application.
	 * @return a boolean value indicating whether this is a GemFire cache server application.
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerApplication
	 * @see #isTypedCacheApplication(Class, AnnotationMetadata)
	 */
	protected boolean isCacheServerApplication(AnnotationMetadata importMetadata) {
		return isTypedCacheApplication(CacheServerApplication.class, importMetadata);
	}

	/**
	 * Determines whether this is a GemFire {@link org.apache.geode.cache.client.ClientCache} application,
	 * which is indicated by the presence of the {@link ClientCacheApplication} annotation on a Spring application
	 * {@link org.springframework.context.annotation.Configuration @Configuration} class.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing application configuration meta-data
	 * from the annotations used to configure the Spring application.
	 * @return a boolean value indicating whether this is a GemFire cache client application.
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
	 * @see #isTypedCacheApplication(Class, AnnotationMetadata)
	 */
	protected boolean isClientCacheApplication(AnnotationMetadata importMetadata) {
		return isTypedCacheApplication(ClientCacheApplication.class, importMetadata);
	}

	/**
	 * Determines whether this is a GemFire peer {@link org.apache.geode.cache.Cache} application,
	 * which is indicated by the presence of the {@link PeerCacheApplication} annotation on a Spring application
	 * {@link org.springframework.context.annotation.Configuration @Configuration} class.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing application configuration meta-data
	 * from the annotations used to configure the Spring application.
	 * @return a boolean value indicating whether this is a GemFire peer cache application.
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
	 * @see #isTypedCacheApplication(Class, AnnotationMetadata)
	 */
	protected boolean isPeerCacheApplication(AnnotationMetadata importMetadata) {
		return isTypedCacheApplication(PeerCacheApplication.class, importMetadata);
	}

	/**
	 * Determines whether this Spring application is annotated with the given GemFire cache type annotation.
	 *
	 * @param annotationType {@link Annotation} cache type.
	 * @param importMetadata {@link AnnotationMetadata} containing application configuration meta-data
	 * from the annotations used to configure the Spring application.
	 * @return a boolean value indicating if this Spring application is annotated with the given GemFire
	 * cache type annotation.
	 * @see org.springframework.core.type.AnnotationMetadata
	 * @see java.lang.annotation.Annotation
	 * @see #getAnnotationTypeName()
	 * @see #getAnnotationType()
	 */
	protected boolean isTypedCacheApplication(Class<? extends Annotation> annotationType,
			AnnotationMetadata importMetadata) {

		return (annotationType.equals(getAnnotationType()) && importMetadata.hasAnnotation(getAnnotationTypeName()));
	}

	/**
	 * Determines whether this is a GemFire {@link org.apache.geode.cache.server.CacheServer} or
	 * {@link org.apache.geode.cache.Cache peer cache} application, which is indicated by the presence
	 * of either the {@link CacheServerApplication} annotation or the {@link PeerCacheApplication} annotation
	 * on a Spring application {@link org.springframework.context.annotation.Configuration @Configuration} class.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing application configuration meta-data
	 * from the annotations used to configure the Spring application.
	 * @return a boolean value indicating whether this is a GemFire cache server or peer cache application.
	 * @see org.springframework.core.type.AnnotationMetadata
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerApplication
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
	 * @see #isCacheServerApplication(AnnotationMetadata)
	 * @see #isPeerCacheApplication(AnnotationMetadata)
	 */
	protected boolean isCacheServerOrPeerCacheApplication(AnnotationMetadata importMetadata) {
		return (isCacheServerApplication(importMetadata) || isPeerCacheApplication(importMetadata));
	}

	/**
	 * Determine whether this Spring application is a {@link org.apache.geode.cache.server.CacheServer},
	 * {@link org.apache.geode.cache.client.ClientCache} or a {@link org.apache.geode.cache.Cache} application.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing application configuration meta-data
	 * from the class type-level annotations used to configure the Spring application.
	 * @return a boolean value indicating whether this is a GemFire cache server, client cache or peer cache
	 * Spring application.
	 * @see #isCacheServerApplication(AnnotationMetadata)
	 * @see #isClientCacheApplication(AnnotationMetadata)
	 * @see #isPeerCacheApplication(AnnotationMetadata)
	 */
	protected boolean isClientPeerOrServerCacheApplication(AnnotationMetadata importMetadata) {
		return (isCacheServerApplication(importMetadata) || isClientCacheApplication(importMetadata)
			|| isPeerCacheApplication(importMetadata));
	}

	/* (non-Javadoc) */
	void setCacheXml(Resource cacheXml) {
		this.cacheXml = cacheXml;
	}

	protected Resource getCacheXml() {
		return this.cacheXml;
	}

	/* (non-Javadoc) */
	void setClose(boolean close) {
		this.close = close;
	}

	protected boolean isClose() {
		return this.close;
	}

	/* (non-Javadoc) */
	void setCopyOnRead(boolean copyOnRead) {
		this.copyOnRead = copyOnRead;
	}

	protected boolean getCopyOnRead() {
		return this.copyOnRead;
	}

	/* (non-Javadoc) */
	void setCriticalHeapPercentage(Float criticalHeapPercentage) {
		this.criticalHeapPercentage = criticalHeapPercentage;
	}

	protected Float getCriticalHeapPercentage() {
		return this.criticalHeapPercentage;
	}

	/* (non-Javadoc) */
	void setDynamicRegionSupport(DynamicRegionSupport dynamicRegionSupport) {
		this.dynamicRegionSupport = dynamicRegionSupport;
	}

	protected DynamicRegionSupport getDynamicRegionSupport() {
		return this.dynamicRegionSupport;
	}

	/* (non-Javadoc) */
	void setEvictionHeapPercentage(Float evictionHeapPercentage) {
		this.evictionHeapPercentage = evictionHeapPercentage;
	}

	protected Float getEvictionHeapPercentage() {
		return this.evictionHeapPercentage;
	}

	/* (non-Javadoc) */
	void setGatewayConflictResolver(GatewayConflictResolver gatewayConflictResolver) {
		this.gatewayConflictResolver = gatewayConflictResolver;
	}

	protected GatewayConflictResolver getGatewayConflictResolver() {
		return this.gatewayConflictResolver;
	}

	/* (non-Javadoc) */
	void setJndiDataSources(List<JndiDataSource> jndiDataSources) {
		this.jndiDataSources = jndiDataSources;
	}

	protected List<CacheFactoryBean.JndiDataSource> getJndiDataSources() {
		return nullSafeList(this.jndiDataSources);
	}

	/* (non-Javadoc) */
	void setLocators(String locators) {
		this.locators = locators;
		this.mcastPort = DEFAULT_MCAST_PORT;
	}

	protected String locators() {
		return this.locators;
	}

	/* (non-Javadoc) */
	void setLogLevel(String logLevel) {
		this.logLevel = logLevel;
	}

	protected String logLevel() {
		return Optional.ofNullable(this.logLevel).orElse(DEFAULT_LOG_LEVEL);
	}

	/* (non-Javadoc) */
	void setMappingContext(GemfireMappingContext mappingContext) {
		this.mappingContext = mappingContext;
	}

	protected GemfireMappingContext mappingContext() {
		return this.mappingContext;
	}

	void setMcastPort(Integer mcastPort) {
		this.mcastPort = mcastPort;
		this.locators = DEFAULT_LOCATORS;
	}

	protected Integer mcastPort() {
		return Optional.ofNullable(mcastPort).orElse(DEFAULT_MCAST_PORT);
	}

	/* (non-Javadoc) */
	void setName(String name) {
		this.name = name;
	}

	protected String name() {
		return Optional.ofNullable(this.name).filter(StringUtils::hasText).orElseGet(this::toString);
	}

	/* (non-Javadoc) */
	void setPdxDiskStoreName(String pdxDiskStoreName) {
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	protected String getPdxDiskStoreName() {
		return this.pdxDiskStoreName;
	}

	/* (non-Javadoc) */
	void setPdxIgnoreUnreadFields(Boolean pdxIgnoreUnreadFields) {
		this.pdxIgnoreUnreadFields = pdxIgnoreUnreadFields;
	}

	protected Boolean getPdxIgnoreUnreadFields() {
		return this.pdxIgnoreUnreadFields;
	}

	/* (non-Javadoc) */
	void setPdxPersistent(Boolean pdxPersistent) {
		this.pdxPersistent = pdxPersistent;
	}

	protected Boolean getPdxPersistent() {
		return this.pdxPersistent;
	}

	/* (non-Javadoc) */
	void setPdxReadSerialized(Boolean pdxReadSerialized) {
		this.pdxReadSerialized = pdxReadSerialized;
	}

	protected Boolean getPdxReadSerialized() {
		return this.pdxReadSerialized;
	}

	/* (non-Javadoc) */
	void setPdxSerializer(PdxSerializer pdxSerializer) {
		this.pdxSerializer = pdxSerializer;
	}

	protected PdxSerializer getPdxSerializer() {
		return this.pdxSerializer;
	}

	/* (non-Javadoc) */
	void setStartLocator(String startLocator) {
		this.startLocator = startLocator;
	}

	protected String startLocator() {
		return this.startLocator;
	}

	/* (non-Javadoc) */
	void setTransactionListeners(List<TransactionListener> transactionListeners) {
		this.transactionListeners = transactionListeners;
	}

	protected List<TransactionListener> getTransactionListeners() {
		return nullSafeList(this.transactionListeners);
	}

	/* (non-Javadoc) */
	void setTransactionWriter(TransactionWriter transactionWriter) {
		this.transactionWriter = transactionWriter;
	}

	protected TransactionWriter getTransactionWriter() {
		return this.transactionWriter;
	}

	/* (non-Javadoc) */
	void setUseBeanFactoryLocator(boolean useBeanFactoryLocator) {
		this.useBeanFactoryLocator = useBeanFactoryLocator;
	}

	protected boolean useBeanFactoryLocator() {
		return this.useBeanFactoryLocator;
	}

	public void add(Properties gemfireProperties) {
		customGemFireProperties.add(gemfireProperties);
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString() {
		return DEFAULT_NAME;
	}
}
