/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.springframework.beans.BeanInstantiationException;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.cache.annotation.Caching;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.annotation.support.BeanDefinitionRegistryPostProcessorSupport;
import org.springframework.data.gemfire.config.annotation.support.GemFireCacheTypeAwareRegionFactoryBean;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;

/**
 * The {@link CachingDefinedRegionsConfiguration} class is a Spring {@link Configuration @Configuration} class
 * that applies configuration to a Spring (Data GemFire/Geode) application to create GemFire/Geode cache
 * {@link Region Regions} based on the use of Spring's Cache Abstraction to enable caching for application
 * service classes and methods.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.reflect.AnnotatedElement
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.beans.factory.annotation.AnnotatedBeanDefinition
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor
 * @see org.springframework.cache.annotation.CacheConfig
 * @see org.springframework.cache.annotation.CacheEvict
 * @see org.springframework.cache.annotation.CachePut
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.cache.annotation.Caching
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.core.annotation.AnnotatedElementUtils
 * @see org.springframework.core.annotation.AnnotationUtils
 * @see org.springframework.data.gemfire.config.annotation.EnableCachingDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.config.annotation.support.BeanDefinitionRegistryPostProcessorSupport
 * @see org.springframework.data.gemfire.config.annotation.support.GemFireCacheTypeAwareRegionFactoryBean
 * @since 2.0.0
 */
@Configuration
public class CachingDefinedRegionsConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	private static final Class[] CLASS_CACHE_ANNOTATION_TYPES;

	private static final Class[] METHOD_CACHE_ANNOTATION_TYPES =
		asArray(Cacheable.class, CacheEvict.class, CachePut.class);

	static {

		List<Class> annotationTypes = new ArrayList<>();

		Collections.addAll(annotationTypes, METHOD_CACHE_ANNOTATION_TYPES);
		annotationTypes.add(CacheConfig.class);

		CLASS_CACHE_ANNOTATION_TYPES = annotationTypes.toArray(new Class[annotationTypes.size()]);

	}

	private ClientRegionShortcut clientRegionShortcut = ClientRegionShortcut.PROXY;

	private RegionShortcut serverRegionShortcut = RegionShortcut.PARTITION;

	private String poolName = ClientRegionFactoryBean.DEFAULT_POOL_NAME;

	/**
	 * Returns the {@link Annotation} {@link Class type} that configures and creates {@link Region Regions}
	 * for application service {@link Method Methods} that are annotated with Spring's Cache Abstraction Annotations.
	 *
	 * @return the {@link Annotation} {@link Class type} that configures and creates {@link Region Regions}
	 * for application service {@link Method Methods} that are annotated with Spring's Cache Abstraction Annotations.
	 * @see org.springframework.data.gemfire.config.annotation.EnableCachingDefinedRegions
	 * @see java.lang.annotation.Annotation
	 * @see java.lang.Class
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableCachingDefinedRegions.class;
	}

	/**
	 * Configures the {@link ClientRegionShortcut} specifying the data management policy to use
	 * when creating a client {@link Region}.
	 *
	 * @param clientRegionShortcut {@link ClientRegionShortcut} specifying the data management policy
	 * to use when creating a client {@link Region}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 */
	public void setClientRegionShortcut(ClientRegionShortcut clientRegionShortcut) {
		this.clientRegionShortcut = clientRegionShortcut;
	}

	/**
	 * Returns the configured {@link ClientRegionShortcut} specifying the data management policy to use
	 * when creating a client {@link Region}.
	 *
	 * @return an {@link Optional} {@link ClientRegionShortcut} specifying the data management policy to use
	 * when creating a client {@link Region}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see #setClientRegionShortcut(ClientRegionShortcut)
	 * @see java.util.Optional
	 */
	protected Optional<ClientRegionShortcut> getClientRegionShortcut() {
		return Optional.ofNullable(this.clientRegionShortcut);
	}

	/**
	 * Resolves the {@link ClientRegionShortcut} specifying the data management policy to use
	 * when creating a client {@link Region}; defaults to {@link ClientRegionShortcut#PROXY}.
	 *
	 * @return the resolved {@link ClientRegionShortcut} specifying the data management policy to use
	 * when creating a client {@link Region}; defaults to {@link ClientRegionShortcut#PROXY}.
	 * @see org.apache.geode.cache.client.ClientRegionShortcut
	 * @see #getClientRegionShortcut()
	 */
	protected ClientRegionShortcut resolveClientRegionShortcut() {
		return getClientRegionShortcut().orElse(ClientRegionShortcut.PROXY);
	}

	/**
	 * Configures the name of the dedicated {@link Pool} used by all caching-defined client {@link Region Regions}
	 * to send and receive data between the client and server.
	 *
	 * @param poolName {@link String} containing the name of the dedicated {@link Pool} for all
	 * caching-defined client {@link Region Regions}.
	 */
	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	/**
	 * Returns the name of the dedicated {@link Pool} used by all caching-defined client {@link Region Regions}
	 * to send and receive data between the client and server.
	 *
	 * @return an {@link Optional} {@link String name} of the dedicated {@link Pool} used by all caching-defined
	 * client {@link Region Regions}.
	 * @see #setPoolName(String)
	 * @see java.util.Optional
	 */
	protected Optional<String> getPoolName() {
		return Optional.ofNullable(this.poolName).filter(StringUtils::hasText);
	}

	/**
	 * Resolves the name of the dedicated {@link Pool} used by all caching-defined client {@link Region Regions}
	 * to send and receive data between the client and server; defaults to {@literal DEFAULT}.
	 *
	 * @return the {@link String name} of the dedicated {@link Pool} used by all caching-defined
	 * client {@link Region Regions}; defaults to {@literal DEFAULT}.
	 * @see #getPoolName()
	 */
	protected String resolvePoolName() {
		return getPoolName().orElse(ClientRegionFactoryBean.DEFAULT_POOL_NAME);
	}

	/**
	 * Configures the {@link RegionShortcut} specifying the data management policy to use
	 * when creating a server (peer) {@link Region}.
	 *
	 * @param serverRegionShortcut {@link RegionShortcut} specifying the data management policy to use
	 * when creating a server (peer) {@link Region}.
	 * @see org.apache.geode.cache.RegionShortcut
	 */
	public void setServerRegionShortcut(RegionShortcut serverRegionShortcut) {
		this.serverRegionShortcut = serverRegionShortcut;
	}

	/**
	 * Returns the configured {@link RegionShortcut} specifying the data management policy to use
	 * when creating a server (peer) {@link Region}.
	 *
	 * @return an {@link Optional} {@link RegionShortcut} specifying the data management policy to use
	 * when creating a server (peer) {@link Region}.
	 * @see #setServerRegionShortcut(RegionShortcut)
	 * @see org.apache.geode.cache.RegionShortcut
	 * @see java.util.Optional
	 */
	protected Optional<RegionShortcut> getServerRegionShortcut() {
		return Optional.ofNullable(this.serverRegionShortcut);
	}

	/**
	 * Resolves the {@link RegionShortcut} specifying the data management policy to use
	 * when creating a server (peer) {@link Region}; defaults to {@link RegionShortcut#PARTITION}.
	 *
	 * @return the resolved {@link RegionShortcut} specifying the data management policy to use
	 * when creating a server (peer) {@link Region}; defaults to {@link RegionShortcut#PARTITION}.
	 * @see org.apache.geode.cache.RegionShortcut
	 * @see #getServerRegionShortcut()
	 */
	protected RegionShortcut resolveServerRegionShortcut() {
		return getServerRegionShortcut().orElse(RegionShortcut.PARTITION);
	}

	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		if (isAnnotationPresent(importMetadata)) {

			AnnotationAttributes enableCachingDefinedRegionsAttributes = getAnnotationAttributes(importMetadata);

			setClientRegionShortcut(enableCachingDefinedRegionsAttributes.getEnum("clientRegionShortcut"));

			setPoolName(enableCachingDefinedRegionsAttributes.getString("poolName"));

			setServerRegionShortcut(enableCachingDefinedRegionsAttributes.getEnum("serverRegionShortcut"));
		}
	}

	@Bean
	@SuppressWarnings("all")
	public BeanDefinitionRegistryPostProcessor cacheAbstractionAnnotationsRegionBeanDefinitionRegistrar() {

		return new BeanDefinitionRegistryPostProcessorSupport() {

			@Override
			public void postProcessBeanDefinitionRegistry(BeanDefinitionRegistry registry) throws BeansException {
				registerBeanDefinitions(registry);
			}
		};
	}

	@Bean
	@SuppressWarnings("all")
	public BeanPostProcessor cacheAbstractionAnnotationsRegionBeanRegistrar(ConfigurableBeanFactory beanFactory) {

		return new BeanPostProcessor() {

			@Nullable @Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

				if (isNotInfrastructureBean(bean)) {
					registerRegionBeans(collectCacheNames(bean.getClass()), beanFactory);
				}

				return bean;
			}
		};
	}

	void registerBeanDefinitions(BeanDefinitionRegistry registry) {

		for (String beanName : registry.getBeanDefinitionNames()) {

			BeanDefinition beanDefinition = registry.getBeanDefinition(beanName);

			if (isNotInfrastructureBean(beanDefinition)) {
				resolveBeanClass(beanDefinition, registry).ifPresent(beanClass ->
					registerRegionBeanDefinitions(collectCacheNames(beanClass), registry));
			}
		}
	}

	@SuppressWarnings("unchecked")
	private Set<String> collectCacheNames(Class<?> type) {

		Set<String> cacheNames = new HashSet<>();

		cacheNames.addAll(collectCachingCacheNames(type));
		cacheNames.addAll(collectCacheNames(type, CLASS_CACHE_ANNOTATION_TYPES));

		stream(type.getMethods()).forEach(method -> {
			if (isUserLevelMethod(method)) {
				cacheNames.addAll(collectCachingCacheNames(method));
				cacheNames.addAll(collectCacheNames(method, METHOD_CACHE_ANNOTATION_TYPES));
			}
		});

		return cacheNames;
	}

	@SuppressWarnings("all")
	Set<String> collectCacheNames(AnnotatedElement annotatedElement,
			Class<? extends Annotation>... annotationTypes) {

		Stream<String> cacheNames = stream(nullSafeArray(annotationTypes, Class.class))
			.map(annotationType -> resolveAnnotation(annotatedElement, annotationType))
			.flatMap(annotation -> collectCacheNames((Annotation) annotation).stream());

		return cacheNames.collect(Collectors.toSet());
	}

	private Set<String> collectCacheNames(Annotation annotation) {

		return Optional.ofNullable(annotation)
			.map(this::getAnnotationAttributes)
			.map(annotationAttributes -> annotationAttributes.getStringArray("cacheNames"))
			.map(CollectionUtils::asSet)
			.orElse(Collections.emptySet());
	}

	private Set<String> collectCachingCacheNames(AnnotatedElement annotatedElement) {

		Set<String> cacheNames = new HashSet<>();

		Optional.ofNullable(resolveAnnotation(annotatedElement, Caching.class))
			.ifPresent(caching -> {

				cacheNames.addAll(stream(nullSafeArray(caching.cacheable(), Cacheable.class))
					.flatMap(cacheable -> collectCacheNames(cacheable).stream())
					.collect(Collectors.toSet()));

				cacheNames.addAll(stream(nullSafeArray(caching.evict(), CacheEvict.class))
					.flatMap(cacheEvict -> collectCacheNames(cacheEvict).stream())
					.collect(Collectors.toSet()));

				cacheNames.addAll(stream(nullSafeArray(caching.put(), CachePut.class))
					.flatMap(cachePut -> collectCacheNames(cachePut).stream())
					.collect(Collectors.toSet()));

			});

		return cacheNames;
	}

	private BeanDefinitionRegistry registerRegionBeanDefinitions(Set<String> cacheNames,
			BeanDefinitionRegistry registry) {

		cacheNames.forEach(cacheName -> {

			if (!registry.containsBeanDefinition(cacheName)) {

				BeanDefinitionBuilder builder =
					BeanDefinitionBuilder.genericBeanDefinition(GemFireCacheTypeAwareRegionFactoryBean.class);

				builder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);
				builder.addPropertyValue("clientRegionShortcut", resolveClientRegionShortcut());
				builder.addPropertyValue("poolName", resolvePoolName());
				builder.addPropertyValue("regionName", cacheName);
				builder.addPropertyValue("serverRegionShortcut", resolveServerRegionShortcut());

				registry.registerBeanDefinition(cacheName, builder.getBeanDefinition());
			}
		});

		return registry;
	}

	private ConfigurableBeanFactory registerRegionBeans(Set<String> cacheNames, ConfigurableBeanFactory beanFactory) {

		cacheNames.forEach(cacheName -> {

			if (!beanFactory.containsBean(cacheName)) {
				try {

					GemFireCacheTypeAwareRegionFactoryBean<?, ?> regionFactoryBean =
						new GemFireCacheTypeAwareRegionFactoryBean<>();

					GemFireCache gemfireCache =
						beanFactory.getBean(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME, GemFireCache.class);

					regionFactoryBean.setCache(gemfireCache);
					regionFactoryBean.setClientRegionShortcut(resolveClientRegionShortcut());
					regionFactoryBean.setPoolName(resolvePoolName());
					regionFactoryBean.setRegionName(cacheName);
					regionFactoryBean.setServerRegionShortcut(resolveServerRegionShortcut());
					regionFactoryBean.afterPropertiesSet();

					Optional.ofNullable(regionFactoryBean.getObject())
						.ifPresent(region -> beanFactory.registerSingleton(cacheName, region));
				}
				catch (Exception cause) {
					throw new BeanInstantiationException(Region.class,
						String.format("Failed to create Region for cache [%s]", cacheName), cause);
				}
			}
		});

		return beanFactory;
	}
}
