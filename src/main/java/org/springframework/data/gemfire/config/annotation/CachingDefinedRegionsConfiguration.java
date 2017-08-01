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
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

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
import org.springframework.beans.BeanInstantiationException;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
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
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.type.MethodMetadata;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.config.annotation.support.BeanDefinitionRegistryPostProcessorSupport;
import org.springframework.data.gemfire.config.annotation.support.GemFireCacheTypeAwareRegionFactoryBean;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

/**
 * The {@link CachingDefinedRegionsConfiguration} class is a Spring {@link Configuration @Configuration} class
 * that applies configuration to a Spring (Data GemFire/Geode) application to create GemFire/Geode cache
 * {@link Region Regions} based on the use of Spring's Cache Abstraction to enable caching for application
 * service classes and methods.
 *
 * @author John Blum
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.BeanPostProcessor
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
 * @see org.springframework.core.annotation.AnnotatedElementUtils
 * @see org.springframework.core.annotation.AnnotationUtils
 * @see org.springframework.data.gemfire.config.annotation.EnableCachingDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.support.BeanDefinitionRegistryPostProcessorSupport
 * @see org.springframework.data.gemfire.config.annotation.support.GemFireCacheTypeAwareRegionFactoryBean
 * @since 2.0.0
 */
@Configuration
public class CachingDefinedRegionsConfiguration {

	private static final Class[] CLASS_CACHE_ANNOTATION_TYPES;

	private static final Class[] METHOD_CACHE_ANNOTATION_TYPES =
		asArray(Cacheable.class, CacheEvict.class, CachePut.class);

	static {

		List<Class> annotationTypes = new ArrayList<>();

		Collections.addAll(annotationTypes, METHOD_CACHE_ANNOTATION_TYPES);
		annotationTypes.add(CacheConfig.class);

		CLASS_CACHE_ANNOTATION_TYPES = annotationTypes.toArray(new Class[annotationTypes.size()]);

	}

	private static final Set<Integer> INFRASTRUCTURE_ROLES =
		asSet(BeanDefinition.ROLE_INFRASTRUCTURE, BeanDefinition.ROLE_SUPPORT);

	private static final String ORG_SPRINGFRAMEWORK_DATA_GEMFIRE_PACKAGE = "org.springframework.data.gemfire";
	private static final String ORG_SPRINGFRAMEWORK_PACKAGE = "org.springframework";

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

	private boolean isNotInfrastructureBean(Object bean) {
		return isNotInfrastructureClass(bean.getClass().getName());
	}

	boolean isNotInfrastructureBean(BeanDefinition beanDefinition) {
		return (isNotInfrastructureRole(beanDefinition) && isNotInfrastructureClass(beanDefinition));
	}

	boolean isNotInfrastructureClass(BeanDefinition beanDefinition) {
		return resolveBeanClassName(beanDefinition).filter(this::isNotInfrastructureClass).isPresent();
	}

	boolean isNotInfrastructureClass(String className) {
		return (className.startsWith(ORG_SPRINGFRAMEWORK_DATA_GEMFIRE_PACKAGE)
			|| !className.startsWith(ORG_SPRINGFRAMEWORK_PACKAGE));
	}

	boolean isNotInfrastructureRole(BeanDefinition beanDefinition) {
		return !INFRASTRUCTURE_ROLES.contains(beanDefinition.getRole());
	}

	boolean isUserLevelMethod(Method method) {

		return Optional.ofNullable(method)
			.filter(ClassUtils::isUserLevelMethod)
			.filter(it -> !Object.class.equals(it.getDeclaringClass()))
			.isPresent();
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
			.map(AnnotationUtils::getAnnotationAttributes)
			.map(annotationAttributes -> (String[]) annotationAttributes.get("cacheNames"))
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
					.flatMap(cacheable -> collectCacheNames(cacheable).stream())
					.collect(Collectors.toSet()));

				cacheNames.addAll(stream(nullSafeArray(caching.put(), CachePut.class))
					.flatMap(cacheable -> collectCacheNames(cacheable).stream())
					.collect(Collectors.toSet()));

			});

		return cacheNames;
	}

	private BeanDefinitionRegistry registerRegionBeanDefinitions(Set<String> cacheNames,
			BeanDefinitionRegistry registry) {

		boolean containsGemFirePoolBeanDefinition =
			registry.containsBeanDefinition(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);

		cacheNames.forEach(cacheName -> {

			if (!registry.containsBeanDefinition(cacheName)) {

				BeanDefinitionBuilder builder =
					BeanDefinitionBuilder.genericBeanDefinition(GemFireCacheTypeAwareRegionFactoryBean.class);

				builder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

				if (!containsGemFirePoolBeanDefinition) {
					builder.addPropertyValue("poolName", GemfireUtils.DEFAULT_POOL_NAME);
				}

				builder.addPropertyValue("regionName", cacheName);
				builder.addPropertyValue("serverRegionShortcut", RegionShortcut.PARTITION);

				registry.registerBeanDefinition(cacheName, builder.getBeanDefinition());
			}
		});

		return registry;
	}

	private ConfigurableBeanFactory registerRegionBeans(Set<String> cacheNames, ConfigurableBeanFactory beanFactory) {

		boolean containsGemFirePoolBean = beanFactory.containsBean(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);

		cacheNames.forEach(cacheName -> {

			if (!beanFactory.containsBean(cacheName)) {
				try {

					GemFireCacheTypeAwareRegionFactoryBean<?, ?> regionFactoryBean =
						new GemFireCacheTypeAwareRegionFactoryBean<>();

					regionFactoryBean.setCache(beanFactory.getBean("gemfireCache", GemFireCache.class));

					if (!containsGemFirePoolBean) {
						regionFactoryBean.setPoolName(GemfireUtils.DEFAULT_POOL_NAME);
					}

					regionFactoryBean.setRegionName(cacheName);
					regionFactoryBean.setServerRegionShortcut(RegionShortcut.PARTITION);
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

	/* (non-Javadoc) */
	<A extends Annotation> A resolveAnnotation(AnnotatedElement annotatedElement, Class<A> annotationType) {

		return (annotatedElement instanceof Class
			? AnnotatedElementUtils.findMergedAnnotation(annotatedElement, annotationType)
			: AnnotationUtils.findAnnotation(annotatedElement, annotationType));
	}

	/* (non-Javadoc) */
	Optional<Class<?>> resolveBeanClass(BeanDefinition beanDefinition, BeanDefinitionRegistry registry) {
		return resolveBeanClass(beanDefinition, resolveBeanClassLoader(registry));
	}

	/* (non-Javadoc) */
	private Optional<Class<?>> resolveBeanClass(BeanDefinition beanDefinition, ClassLoader classLoader) {

		Class<?> beanClass = (beanDefinition instanceof AbstractBeanDefinition
			? safeResolveType(() -> ((AbstractBeanDefinition) beanDefinition).resolveBeanClass(classLoader)) : null);

		if (beanClass == null) {
			beanClass = resolveBeanClassName(beanDefinition).map(beanClassName ->
				safeResolveType(() -> ClassUtils.forName(beanClassName, classLoader))).orElse(null);
		}

		return Optional.ofNullable(beanClass);
	}

	/* (non-Javadoc) */
	ClassLoader resolveBeanClassLoader(BeanDefinitionRegistry registry) {

		return (registry instanceof ConfigurableBeanFactory
			? ((ConfigurableBeanFactory) registry).getBeanClassLoader()
			: Thread.currentThread().getContextClassLoader());
	}

	/* (non-Javadoc) */
	Optional<String> resolveBeanClassName(BeanDefinition beanDefinition) {

		Optional<String> beanClassName =
			Optional.ofNullable(beanDefinition.getBeanClassName()).filter(StringUtils::hasText);

		if (!beanClassName.isPresent()) {
			beanClassName = Optional.of(beanDefinition)
				.filter(it -> StringUtils.hasText(it.getFactoryMethodName()))
				.filter(it -> it instanceof AnnotatedBeanDefinition)
				.map(it -> ((AnnotatedBeanDefinition) it).getFactoryMethodMetadata())
				.map(MethodMetadata::getReturnTypeName);
		}

		return beanClassName;
	}

	/* (non-Javadoc) */
	Class<?> safeResolveType(TypeResolver typeResolver) {
		try {
			return typeResolver.resolve();
		}
		catch (ClassNotFoundException cause) {
			return null;
		}
	}

	interface TypeResolver {
		Class<?> resolve() throws ClassNotFoundException;
	}
}
