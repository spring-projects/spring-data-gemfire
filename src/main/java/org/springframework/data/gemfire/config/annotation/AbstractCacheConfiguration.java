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
import static org.springframework.data.gemfire.util.SpringUtils.defaultIfNull;

import java.lang.annotation.Annotation;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import com.gemstone.gemfire.cache.TransactionListener;
import com.gemstone.gemfire.cache.TransactionWriter;
import com.gemstone.gemfire.cache.util.GatewayConflictResolver;
import com.gemstone.gemfire.pdx.PdxSerializer;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.io.Resource;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.config.support.CustomEditorBeanFactoryPostProcessor;
import org.springframework.data.gemfire.config.support.DefinedIndexesApplicationListener;
import org.springframework.data.gemfire.config.support.DiskStoreDirectoryBeanPostProcessor;
import org.springframework.data.gemfire.config.support.PdxDiskStoreAwareBeanFactoryPostProcessor;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.MappingPdxSerializer;
import org.springframework.data.gemfire.util.PropertiesBuilder;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * {@link AbstractCacheConfiguration} is an abstract base class for configuring either a Pivotal GemFire/Apache Geode
 * client or peer-based cache instance using Spring's Java-based, Annotation
 * {@link org.springframework.context.annotation.Configuration} support.
 *
 * This class encapsulates configuration settings common to both GemFire peer
 * {@link com.gemstone.gemfire.cache.Cache caches} and
 * {@link com.gemstone.gemfire.cache.client.ClientCache client caches}.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.io.Resource
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @since 1.9.0
 */
@Configuration
@SuppressWarnings("unused")
public abstract class AbstractCacheConfiguration implements BeanClassLoaderAware, BeanFactoryAware, ImportAware {

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

	private BeanFactory beanFactory;

	private Boolean pdxIgnoreUnreadFields;
	private Boolean pdxPersistent;
	private Boolean pdxReadSerialized;

	private ClassLoader beanClassLoader;

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
	 * Determines whether the given {@link Object} has value.  The {@link Object} is valuable
	 * if it is not {@literal null}.
	 *
	 * @param value {@link Object} to evaluate.
	 * @return a boolean value indicating whether the given {@link Object} has value.
	 */
	protected static boolean hasValue(Object value) {
		return (value != null);
	}

	/**
	 * Determines whether the given {@link Number} has value.  The {@link Number} is valuable
	 * if it is not {@literal null} and is not equal to 0.0d.
	 *
	 * @param value {@link Number} to evaluate.
	 * @return a boolean value indicating whether the given {@link Number} has value.
	 */
	protected static boolean hasValue(Number value) {
		return (value != null && value.doubleValue() != 0.0d);
	}

	/**
	 * Determines whether the given {@link String} has value.  The {@link String} is valuable
	 * if it is not {@literal null} or empty.
	 *
	 * @param value {@link String} to evaluate.
	 * @return a boolean value indicating whether the given {@link String} is valuable.
	 */
	protected static boolean hasValue(String value) {
		return StringUtils.hasText(value);
	}

	/**
	 * Returns a {@link Properties} object containing GemFire System properties used to configure the GemFire cache.
	 *
	 * The name of the GemFire member/node in the cluster is set to a default, pre-defined, descriptive value
	 * depending on the type of configuration meta-data applied.
	 *
	 * Both 'mcast-port' and 'locators' are to set 0 and empty String respectively, which is necessary
	 * for {@link com.gemstone.gemfire.cache.client.ClientCache cache client}-based applications.  These values
	 * can be changed for peer cache and cache server applications.
	 *
	 * Finally, GemFire's {@literal log-level} System property defaults to {@literal config}.
	 *
	 * @return a {@link Properties} object containing GemFire System properties used to configure the GemFire cache.
	 * @see <a link="http://gemfire.docs.pivotal.io/docs-gemfire/reference/topics/gemfire_properties.html">GemFire Properties</a>
	 * @see java.util.Properties
	 * @see #name()
	 * @see #logLevel()
	 * @see #locators()
	 */
	@Bean
	protected Properties gemfireProperties() {
		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty("name", name());
		gemfireProperties.setProperty("mcast-port", mcastPort());
		gemfireProperties.setProperty("log-level", logLevel());
		gemfireProperties.setProperty("locators", locators());
		gemfireProperties.setProperty("start-locator", startLocator());
		gemfireProperties.add(customGemFireProperties);

		return gemfireProperties.build();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		this.beanClassLoader = beanClassLoader;
	}

	/**
	 * Returns a reference to the {@link ClassLoader} use by the Spring {@link BeanFactory} to load classes
	 * for bean definitions.
	 *
	 * @return the {@link ClassLoader} used by the Spring {@link BeanFactory} to load classes for bean definitions.
	 * @see #setBeanClassLoader(ClassLoader)
	 */
	protected ClassLoader beanClassLoader() {
		return beanClassLoader;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory} in the current application context.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @throws IllegalStateException if the Spring {@link BeanFactory} was not properly initialized.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory beanFactory() {
		Assert.state(this.beanFactory != null, "BeanFactory was not properly initialized");
		return this.beanFactory;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {
		configureInfrastructure(importMetadata);
		configureCache(importMetadata);
		configurePdx(importMetadata);
		configureOther(importMetadata);
	}

	/**
	 * Configures Spring container infrastructure components used by Spring Data GemFire
	 * to enable GemFire to function properly inside a Spring context.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing annotation meta-data
	 * for the Spring GemFire cache application class.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	protected void configureInfrastructure(AnnotationMetadata importMetadata) {
		registerCustomEditorBeanFactoryPostProcessor(importMetadata);
		registerDefinedIndexesApplicationListener(importMetadata);
		registerDiskStoreDirectoryBeanPostProcessor(importMetadata);
	}

	/**
	 * Configures the GemFire cache settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing the cache meta-data used to configure
	 * the GemFire cache.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	protected void configureCache(AnnotationMetadata importMetadata) {
		if (isClientPeerOrServerCacheApplication(importMetadata)) {
			Map<String, Object> cacheMetadataAttributes =
				importMetadata.getAnnotationAttributes(getAnnotationTypeName());

			setCopyOnRead(Boolean.TRUE.equals(cacheMetadataAttributes.get("copyOnRead")));

			Float criticalHeapPercentage = (Float) cacheMetadataAttributes.get("criticalHeapPercentage");

			if (hasValue(criticalHeapPercentage)) {
				setCriticalHeapPercentage(criticalHeapPercentage);
			}

			Float evictionHeapPercentage = (Float) cacheMetadataAttributes.get("evictionHeapPercentage");

			if (hasValue(evictionHeapPercentage)) {
				setEvictionHeapPercentage(evictionHeapPercentage);
			}

			setLogLevel((String) cacheMetadataAttributes.get("logLevel"));
			setName((String) cacheMetadataAttributes.get("name"));
			setUseBeanFactoryLocator(Boolean.TRUE.equals(cacheMetadataAttributes.get("useBeanFactoryLocator")));
		}
	}

	/**
	 * Configures GemFire's PDX Serialization components.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing PDX meta-data used to configure
	 * the GemFire cache with PDX de/serialization capabilities.
	 * @see org.springframework.core.type.AnnotationMetadata
	 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/latest/developing/data_serialization/gemfire_pdx_serialization.html">GemFire PDX Serialization</a>
	 */
	protected void configurePdx(AnnotationMetadata importMetadata) {
		String enablePdxTypeName = EnablePdx.class.getName();

		if (importMetadata.hasAnnotation(enablePdxTypeName)) {
			Map<String, Object> enablePdxAttributes = importMetadata.getAnnotationAttributes(enablePdxTypeName);

			setPdxDiskStoreName((String) enablePdxAttributes.get("diskStoreName"));
			setPdxIgnoreUnreadFields(Boolean.TRUE.equals(enablePdxAttributes.get("ignoreUnreadFields")));
			setPdxPersistent(Boolean.TRUE.equals(enablePdxAttributes.get("persistent")));
			setPdxReadSerialized(Boolean.TRUE.equals(enablePdxAttributes.get("readSerialized")));
			setPdxSerializer(resolvePdxSerializer((String) enablePdxAttributes.get("serializerBeanName")));

			registerPdxDiskStoreAwareBeanFactoryPostProcessor(importMetadata);
		}
	}

	/**
	 * Callback method to configure other, specific GemFire cache configuration settings.
	 *
	 * @param importMetadata {@link AnnotationMetadata} containing meta-data used to configure
	 * the GemFire cache.
	 * @see org.springframework.core.type.AnnotationMetadata
	 */
	protected void configureOther(AnnotationMetadata importMetadata) {
	}

	/* (non-Javadoc) */
	protected PdxSerializer resolvePdxSerializer(String pdxSerializerBeanName) {
		BeanFactory beanFactory = beanFactory();
		PdxSerializer pdxSerializer = pdxSerializer();

		return (beanFactory.containsBean(pdxSerializerBeanName)
			? beanFactory.getBean(pdxSerializerBeanName, PdxSerializer.class)
			: (pdxSerializer != null ? pdxSerializer : newPdxSerializer()));
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected <T extends PdxSerializer> T newPdxSerializer() {
		BeanFactory beanFactory = beanFactory();

		ConversionService conversionService = (beanFactory instanceof ConfigurableBeanFactory
			? ((ConfigurableBeanFactory) beanFactory).getConversionService() : null);

		return (T) MappingPdxSerializer.create(this.mappingContext, conversionService);
	}

	/* (non-Javadoc) */
	protected void registerCustomEditorBeanFactoryPostProcessor(AnnotationMetadata importMetadata) {
		if (CUSTOM_EDITORS_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(CustomEditorBeanFactoryPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/* (non-Javadoc) */
	protected void registerDefinedIndexesApplicationListener(AnnotationMetadata importMetadata) {
		if (DEFINED_INDEXES_APPLICATION_LISTENER_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(DefinedIndexesApplicationListener.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/* (non-Javadoc) */
	protected void registerDiskStoreDirectoryBeanPostProcessor(AnnotationMetadata importMetadata) {
		if (DISK_STORE_DIRECTORY_BEAN_POST_PROCESSOR_REGISTERED.compareAndSet(false, true)) {
			register(BeanDefinitionBuilder.rootBeanDefinition(DiskStoreDirectoryBeanPostProcessor.class)
				.setRole(BeanDefinition.ROLE_INFRASTRUCTURE).getBeanDefinition());
		}
	}

	/* (non-Javadoc) */
	protected void registerPdxDiskStoreAwareBeanFactoryPostProcessor(AnnotationMetadata importMetadata) {
		if (StringUtils.hasText(pdxDiskStoreName())) {
			if (PDX_DISK_STORE_AWARE_BEAN_FACTORY_POST_PROCESSOR_REGISTERED.compareAndSet(false, true)) {
				register(BeanDefinitionBuilder.rootBeanDefinition(PdxDiskStoreAwareBeanFactoryPostProcessor.class)
					.setRole(BeanDefinition.ROLE_INFRASTRUCTURE)
					.addConstructorArgValue(pdxDiskStoreName())
					.getBeanDefinition());
			}
		}
	}

	/**
	 * Registers the given {@link BeanDefinition} with the {@link BeanDefinitionRegistry} using a generated bean name.
	 *
	 * @param beanDefinition {@link AbstractBeanDefinition} to register.
	 * @return the given {@link BeanDefinition}.
	 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.beans.factory.support.BeanDefinitionReaderUtils
	 * 	#registerWithGeneratedName(AbstractBeanDefinition, BeanDefinitionRegistry)
	 * @see #beanFactory()
	 */
	protected AbstractBeanDefinition register(AbstractBeanDefinition beanDefinition) {
		if (beanFactory() instanceof BeanDefinitionRegistry) {
			BeanDefinitionReaderUtils.registerWithGeneratedName(beanDefinition,
				((BeanDefinitionRegistry) beanFactory()));
		}

		return beanDefinition;
	}

	/**
	 * Returns the GemFire cache application {@link java.lang.annotation.Annotation} type pertaining to
	 * this configuration.
	 *
	 * @return the GemFire cache application {@link java.lang.annotation.Annotation} type used by this application.
	 * @see org.springframework.data.gemfire.config.annotation.CacheServerApplication
	 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
	 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
	 */
	protected abstract Class getAnnotationType();

	/**
	 * Returns the fully-qualified class name of the GemFire cache application {@link java.lang.annotation.Annotation}
	 * type.
	 *
	 * @return the fully-qualified class name of the GemFire cache application {@link java.lang.annotation.Annotation}
	 * type.
	 * @see java.lang.Class#getName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/**
	 * Returns the simple class name of the GemFire cache application {@link java.lang.annotation.Annotation} type.
	 *
	 * @return the simple class name of the GemFire cache application {@link java.lang.annotation.Annotation} type.
	 * @see java.lang.Class#getSimpleName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeSimpleName() {
		return getAnnotationType().getSimpleName();
	}

	/**
	 * Determines whether this is a GemFire {@link com.gemstone.gemfire.cache.server.CacheServer} application,
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
	 * Determines whether this is a GemFire {@link com.gemstone.gemfire.cache.client.ClientCache} application,
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
	 * Determines whether this is a GemFire peer {@link com.gemstone.gemfire.cache.Cache} application,
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
	 * Determines whether this is a GemFire {@link com.gemstone.gemfire.cache.server.CacheServer} or
	 * {@link com.gemstone.gemfire.cache.Cache peer cache} application, which is indicated by the presence
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
	 * Determine whether this Spring application is a {@link com.gemstone.gemfire.cache.server.CacheServer},
	 * {@link com.gemstone.gemfire.cache.client.ClientCache} or a {@link com.gemstone.gemfire.cache.Cache} application.
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

	/**
	 * Constructs a new, initialized instance of the {@link CacheFactoryBean} based on the Spring application's
	 * GemFire cache type (i.e. client or peer) preference specified via annotation.
	 *
	 * @param <T> Class type of the {@link CacheFactoryBean}.
	 * @return a new instance of an appropriate {@link CacheFactoryBean} given the Spring application's
	 * GemFire cache type preference  (i.e client or peer) specified with the corresponding annotation
	 * (e.g. {@link ClientCacheApplication} or {@link PeerCacheApplication});
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 * @see #setCommonCacheConfiguration(CacheFactoryBean)
	 * @see #newCacheFactoryBean()
	 */
	protected <T extends CacheFactoryBean> T constructCacheFactoryBean() {
		return setCommonCacheConfiguration(this.<T>newCacheFactoryBean());
	}

	/**
	 * Constructs a new, uninitialized instance of the {@link CacheFactoryBean} based on the Spring application's
	 * GemFire cache type (i.e. client or peer) preference specified via annotation.
	 *
	 * @param <T> Class type of the {@link CacheFactoryBean}.
	 * @return a new instance of an appropriate {@link CacheFactoryBean} given the Spring application's
	 * GemFire cache type preference (i.e client or peer) specified with the corresponding annotation
	 * (e.g. {@link ClientCacheApplication} or {@link PeerCacheApplication}).
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 */
	protected abstract <T extends CacheFactoryBean> T newCacheFactoryBean();

	/**
	 * Configures common GemFire cache configuration settings.
	 *
	 * @param <T> Class type of the {@link CacheFactoryBean}.
	 * @param gemfireCache {@link CacheFactoryBean} instance to configure.
	 * @return the given {@link CacheFactoryBean} after common configuration settings have been applied.
	 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 */
	protected <T extends CacheFactoryBean> T setCommonCacheConfiguration(T gemfireCache) {
		gemfireCache.setBeanClassLoader(beanClassLoader());
		gemfireCache.setBeanFactory(beanFactory());
		gemfireCache.setCacheXml(cacheXml());
		gemfireCache.setClose(close());
		gemfireCache.setCopyOnRead(copyOnRead());
		gemfireCache.setCriticalHeapPercentage(criticalHeapPercentage());
		gemfireCache.setDynamicRegionSupport(dynamicRegionSupport());
		gemfireCache.setEvictionHeapPercentage(evictionHeapPercentage());
		gemfireCache.setGatewayConflictResolver(gatewayConflictResolver());
		gemfireCache.setJndiDataSources(jndiDataSources());
		gemfireCache.setProperties(gemfireProperties());
		gemfireCache.setPdxDiskStoreName(pdxDiskStoreName());
		gemfireCache.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields());
		gemfireCache.setPdxPersistent(pdxPersistent());
		gemfireCache.setPdxReadSerialized(pdxReadSerialized());
		gemfireCache.setPdxSerializer(pdxSerializer());
		gemfireCache.setTransactionListeners(transactionListeners());
		gemfireCache.setTransactionWriter(transactionWriter());

		return gemfireCache;
	}

	/* (non-Javadoc) */
	void setCacheXml(Resource cacheXml) {
		this.cacheXml = cacheXml;
	}

	protected Resource cacheXml() {
		return this.cacheXml;
	}

	/* (non-Javadoc) */
	void setClose(boolean close) {
		this.close = close;
	}

	protected boolean close() {
		return this.close;
	}

	/* (non-Javadoc) */
	void setCopyOnRead(boolean copyOnRead) {
		this.copyOnRead = copyOnRead;
	}

	protected boolean copyOnRead() {
		return this.copyOnRead;
	}

	/* (non-Javadoc) */
	void setCriticalHeapPercentage(Float criticalHeapPercentage) {
		this.criticalHeapPercentage = criticalHeapPercentage;
	}

	protected Float criticalHeapPercentage() {
		return this.criticalHeapPercentage;
	}

	/* (non-Javadoc) */
	void setDynamicRegionSupport(DynamicRegionSupport dynamicRegionSupport) {
		this.dynamicRegionSupport = dynamicRegionSupport;
	}

	protected DynamicRegionSupport dynamicRegionSupport() {
		return this.dynamicRegionSupport;
	}

	/* (non-Javadoc) */
	void setEvictionHeapPercentage(Float evictionHeapPercentage) {
		this.evictionHeapPercentage = evictionHeapPercentage;
	}

	protected Float evictionHeapPercentage() {
		return this.evictionHeapPercentage;
	}

	/* (non-Javadoc) */
	void setGatewayConflictResolver(GatewayConflictResolver gatewayConflictResolver) {
		this.gatewayConflictResolver = gatewayConflictResolver;
	}

	protected GatewayConflictResolver gatewayConflictResolver() {
		return this.gatewayConflictResolver;
	}

	/* (non-Javadoc) */
	void setJndiDataSources(List<JndiDataSource> jndiDataSources) {
		this.jndiDataSources = jndiDataSources;
	}

	protected List<CacheFactoryBean.JndiDataSource> jndiDataSources() {
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
		return defaultIfNull(this.logLevel, DEFAULT_LOG_LEVEL);
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
		return (mcastPort != null ? mcastPort : DEFAULT_MCAST_PORT);
	}

	/* (non-Javadoc) */
	void setName(String name) {
		this.name = name;
	}

	protected String name() {
		return (StringUtils.hasText(name) ? name : toString());
	}

	/* (non-Javadoc) */
	void setPdxDiskStoreName(String pdxDiskStoreName) {
		this.pdxDiskStoreName = pdxDiskStoreName;
	}

	protected String pdxDiskStoreName() {
		return this.pdxDiskStoreName;
	}

	/* (non-Javadoc) */
	void setPdxIgnoreUnreadFields(Boolean pdxIgnoreUnreadFields) {
		this.pdxIgnoreUnreadFields = pdxIgnoreUnreadFields;
	}

	protected Boolean pdxIgnoreUnreadFields() {
		return this.pdxIgnoreUnreadFields;
	}

	/* (non-Javadoc) */
	void setPdxPersistent(Boolean pdxPersistent) {
		this.pdxPersistent = pdxPersistent;
	}

	protected Boolean pdxPersistent() {
		return this.pdxPersistent;
	}

	/* (non-Javadoc) */
	void setPdxReadSerialized(Boolean pdxReadSerialized) {
		this.pdxReadSerialized = pdxReadSerialized;
	}

	protected Boolean pdxReadSerialized() {
		return this.pdxReadSerialized;
	}

	/* (non-Javadoc) */
	void setPdxSerializer(PdxSerializer pdxSerializer) {
		this.pdxSerializer = pdxSerializer;
	}

	protected PdxSerializer pdxSerializer() {
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

	protected List<TransactionListener> transactionListeners() {
		return nullSafeList(this.transactionListeners);
	}

	/* (non-Javadoc) */
	void setTransactionWriter(TransactionWriter transactionWriter) {
		this.transactionWriter = transactionWriter;
	}

	protected TransactionWriter transactionWriter() {
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
