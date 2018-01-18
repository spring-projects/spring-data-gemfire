/*
 * Copyright 2017-2018 the original author or authors.
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

import static java.util.Arrays.asList;
import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.StreamUtils.concat;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.cache.annotation.CacheDefaults;
import javax.cache.annotation.CacheRemove;
import javax.cache.annotation.CacheRemoveAll;
import javax.cache.annotation.CacheResult;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.springframework.beans.BeanInstantiationException;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
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
import org.springframework.data.gemfire.config.annotation.support.CacheTypeAwareRegionFactoryBean;
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
 * @see CacheTypeAwareRegionFactoryBean
 * @since 2.0.0
 */
@Configuration
public class CachingDefinedRegionsConfiguration extends AbstractAnnotationConfigSupport implements ImportAware {

	private final CacheNameResolver composableCacheNameResolver = type ->
		asList(new Jsr107CacheAnnotationsCacheNameResolver(), new SpringCacheAnnotationsCacheNameResolver()).stream()
			.flatMap(cacheNameResolver -> cacheNameResolver.resolveCacheNames(type).stream())
			.collect(Collectors.toSet());

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
	 * Returns the configured {@link CacheNameResolver} to resolve all the declared cache name on Spring application
	 * beans/components declared and registered in the Spring container (context).
	 *
	 * @return the configured {@link CacheNameResolver} to resolve all teh caches used by the Spring application.
	 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration.CacheNameResolver
	 */
	protected CacheNameResolver getCacheNameResolver() {
		return this.composableCacheNameResolver;
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
	public BeanPostProcessor cachingAnnotationsRegionBeanRegistrar(ConfigurableBeanFactory beanFactory) {

		return new BeanPostProcessor() {

			@Nullable @Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

				if (isNotInfrastructureBean(bean)) {
					registerRegionBeans(getCacheNameResolver().resolveCacheNames(bean.getClass()), beanFactory);
				}

				return bean;
			}
		};
	}

	private ConfigurableBeanFactory registerRegionBeans(Set<String> cacheNames, ConfigurableBeanFactory beanFactory) {

		cacheNames.forEach(cacheName -> {

			if (!beanFactory.containsBean(cacheName)) {
				try {

					CacheTypeAwareRegionFactoryBean<?, ?> regionFactoryBean = new CacheTypeAwareRegionFactoryBean<>();

					GemFireCache gemfireCache = beanFactory.getBean(GemFireCache.class);

					regionFactoryBean.setCache(gemfireCache);
					regionFactoryBean.setClientRegionShortcut(resolveClientRegionShortcut());
					regionFactoryBean.setRegionName(cacheName);
					regionFactoryBean.setServerRegionShortcut(resolveServerRegionShortcut());

					String poolName = resolvePoolName();

					if (!ClientRegionFactoryBean.DEFAULT_POOL_NAME.equalsIgnoreCase(poolName)) {
						regionFactoryBean.setPoolName(poolName);
					}

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

	/**
	 * {@link CacheNameResolver} is a {@link FunctionalInterface} declaring a contract for all implementations
	 * used to resolve all cache names declared and used by a Spring application.  A resolver typically inspects
	 * all the application beans/components declared and registered in the Spring container (context) setup by
	 * the application to determine whether the application components require caching behavior.
	 *
	 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration.Jsr107CacheAnnotationsCacheNameResolver
	 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration.SpringCacheAnnotationsCacheNameResolver
	 */
	@FunctionalInterface
	protected interface CacheNameResolver {
		Set<String> resolveCacheNames(Class<?> type);
	}

	/**
	 * {@link AbstractCacheNameResolver} is an abstract base class encapsulating reusable functionality common
	 * to all {@link CacheNameResolver} implementations.
	 *
	 * Current implementations support inlude JSR-107, JCache API annotation and Spring's Cache Abstraction annotations.
	 *
	 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration.CacheNameResolver
	 */
	protected abstract class AbstractCacheNameResolver implements CacheNameResolver {

		private final String JSR_107_CACHE_NAME_ATTRIBUTE_NAME = "cacheName";
		private final String SPRING_CACHE_NAMES_ATTRIBUTE_NAME = "cacheNames";

		private final String[] EMPTY_ARRAY = new String[0];

		protected abstract Class<? extends Annotation>[] getClassCacheAnnotationTypes();

		protected abstract Class<? extends Annotation>[] getMethodCacheAnnotationTypes();

		@SuppressWarnings("unchecked")
		protected Class[] append(Class[] annotationTypes, Class... additionalAnnotationTypes) {

			List<Class> annotationTypeList = new ArrayList<>(Arrays.asList(annotationTypes));

			Collections.addAll(annotationTypeList, additionalAnnotationTypes);

			return annotationTypeList.toArray(new Class[annotationTypeList.size()]);
		}

		protected Set<String> resolveCacheNames(Annotation annotation) {

			return Optional.ofNullable(annotation)
				.map(it -> getAnnotationAttributes(it))
				.map(annotationAttributes -> {

					String attributeName = annotationAttributes.containsKey(SPRING_CACHE_NAMES_ATTRIBUTE_NAME)
						? SPRING_CACHE_NAMES_ATTRIBUTE_NAME : JSR_107_CACHE_NAME_ATTRIBUTE_NAME;

					return annotationAttributes.containsKey(attributeName)
						? annotationAttributes.getStringArray(attributeName)
						: EMPTY_ARRAY;

				})
				.map(CollectionUtils::asSet)
				.orElse(Collections.emptySet());
		}

		@Override
		@SuppressWarnings("unchecked")
		public Set<String> resolveCacheNames(Class<?> type) {

			Set<String> cacheNames = new HashSet<>();

			cacheNames.addAll(resolveCacheNames(type, getClassCacheAnnotationTypes()));

			stream(type.getMethods())
				.filter(method -> isUserLevelMethod(method))
				.forEach(method -> cacheNames.addAll(resolveCacheNames(method, getMethodCacheAnnotationTypes())));

			return cacheNames;
		}

		@SuppressWarnings("all")
		protected Set<String> resolveCacheNames(AnnotatedElement annotatedElement,
				Class<? extends Annotation>... annotationTypes) {

			Stream<String> cacheNames = stream(nullSafeArray(annotationTypes, Class.class))
				.map(annotationType -> resolveAnnotation(annotatedElement, annotationType))
				.flatMap(annotation -> resolveCacheNames((Annotation) annotation).stream());

			return cacheNames.collect(Collectors.toSet());
		}
	}

	protected class Jsr107CacheAnnotationsCacheNameResolver extends AbstractCacheNameResolver {

		@Override
		@SuppressWarnings("unchecked")
		protected Class<? extends Annotation>[] getClassCacheAnnotationTypes() {
			return append(getMethodCacheAnnotationTypes(), CacheDefaults.class);
		}

		@Override
		protected Class<? extends Annotation>[] getMethodCacheAnnotationTypes() {
			return asArray(javax.cache.annotation.CachePut.class, CacheRemove.class,
				CacheRemoveAll.class, CacheResult.class);
		}
	}

	protected class SpringCacheAnnotationsCacheNameResolver extends AbstractCacheNameResolver {

		@Override
		@SuppressWarnings("unchecked")
		protected Class<? extends Annotation>[] getClassCacheAnnotationTypes() {
			return append(getMethodCacheAnnotationTypes(), Caching.class);
		}

		@Override
		protected Class<? extends Annotation>[] getMethodCacheAnnotationTypes() {
			return asArray(Cacheable.class, CacheEvict.class, CachePut.class);
		}

		@Override
		@SuppressWarnings("unchecked")
		public Set<String> resolveCacheNames(Class<?> type) {

			Set<String> cacheNames = super.resolveCacheNames(type);

			cacheNames.addAll(resolveCachingCacheNames(type));

			stream(type.getMethods()).filter(method -> isUserLevelMethod(method))
				.forEach(method -> cacheNames.addAll(resolveCachingCacheNames(method)));

			return cacheNames;
		}

		@SuppressWarnings("unchecked")
		private Set<String> resolveCachingCacheNames(AnnotatedElement annotatedElement) {

			Set<String> cacheNames = new HashSet<>();

			Optional.ofNullable(resolveAnnotation(annotatedElement, Caching.class)).ifPresent(caching ->
				concat(stream(caching.cacheable()), stream(caching.evict()), stream(caching.put()))
					.flatMap(cacheAnnotation -> resolveCacheNames(cacheAnnotation).stream())
					.collect(Collectors.toCollection(() -> cacheNames)));

			return cacheNames;
		}
	}
}
