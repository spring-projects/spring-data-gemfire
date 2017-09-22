/*
 * Copyright 2016 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation.support;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.env.Environment;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.core.type.filter.TypeFilter;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.StringUtils;

/**
 * The {@link GemFireComponentClassTypeScanner} class is a classpath component scanner used to search
 * for GemFire components based on {@link Class} type.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider
 * @see org.springframework.core.env.Environment
 * @see org.springframework.core.type.filter.TypeFilter
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class GemFireComponentClassTypeScanner implements Iterable<String> {

	/**
	 * Factory method to construct an instance of the {@link GemFireComponentClassTypeScanner} initialized with
	 * the given array of base packages to scan.
	 *
	 * @param basePackages array of base packages to scan for GemFire components.
	 * @throws IllegalArgumentException if the array of base packages is {@literal null} or empty.
	 * @return an initialized instance of {@link GemFireComponentClassTypeScanner}.
	 * @see #GemFireComponentClassTypeScanner(Set)
	 */
	public static GemFireComponentClassTypeScanner from(String... basePackages) {
		return new GemFireComponentClassTypeScanner(asSet(nullSafeArray(basePackages, String.class)));
	}

	/**
	 * Factory method to construct an instance of the {@link GemFireComponentClassTypeScanner} initialized with
	 * the given {@link Iterable} of base packages to scan.
	 *
	 * @param basePackages {@link Iterable} of base packages to scan for GemFire components.
	 * @throws IllegalArgumentException if the {@link Iterable} of base packages is {@literal null} or empty.
	 * @return an initialized instance of {@link GemFireComponentClassTypeScanner}.
	 * @see #GemFireComponentClassTypeScanner(Set)
	 */
	public static GemFireComponentClassTypeScanner from(Iterable<String> basePackages) {
		return new GemFireComponentClassTypeScanner(stream(basePackages.spliterator(), false)
			.collect(Collectors.toSet()));
	}

	private ClassLoader entityClassLoader;

	private ConfigurableApplicationContext applicationContext;

	private Set<TypeFilter> excludes = new HashSet<>();
	private Set<TypeFilter> includes = new HashSet<>();

	protected final Log log = LogFactory.getLog(getClass());

	private final Set<String> basePackages;

	/**
	 * Constructs an instance of the {@link GemFireComponentClassTypeScanner} initialized with
	 * the given {@link Set} of base packages to scan.
	 *
	 * @param basePackages {@link Set} of base packages to scan for GemFire component clases.
	 * @throws IllegalArgumentException if the {@link Set} is {@literal null} or empty.
	 * @see java.util.Set
	 */
	protected GemFireComponentClassTypeScanner(Set<String> basePackages) {
		Assert.notEmpty(basePackages, "Base packages is required");
		this.basePackages = basePackages;
	}

	/**
	 * Returns a reference to the Spring {@link org.springframework.context.ApplicationContext}.
	 *
	 * @return a reference to the Spring {@link org.springframework.context.ApplicationContext}.
	 * @see org.springframework.context.ConfigurableApplicationContext
	 */
	protected ConfigurableApplicationContext getApplicationContext() {
		return this.applicationContext;
	}

	/**
	 * Returns an unmodifiable {@link Set} of base packages to scan for GemFire components.
	 *
	 * @return an unmodifiable {@link Set} of base packages to scan for GemFire components.
	 * @see java.util.Set
	 */
	protected Set<String> getBasePackages() {
		return Collections.unmodifiableSet(this.basePackages);
	}

	/**
	 * Returns a reference to the {@link ClassLoader} used to find and load GemFire application
	 * persistent entity classes.
	 *
	 * @return the {@link ClassLoader} used to find and load GemFire application persistent entity classes.
	 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory#getBeanClassLoader()
	 * @see java.lang.Thread#getContextClassLoader()
	 * @see java.lang.ClassLoader
	 * @see #getApplicationContext()
	 */
	protected ClassLoader getEntityClassLoader() {

		ConfigurableApplicationContext applicationContext = getApplicationContext();

		return (this.entityClassLoader != null ? this.entityClassLoader
			: (applicationContext != null ? applicationContext.getBeanFactory().getBeanClassLoader()
			: Thread.currentThread().getContextClassLoader()));
	}

	/**
	 * Returns a reference to the Spring {@link Environment} in which the Spring GemFire application is running.
	 *
	 * @return a reference to the Spring {@link Environment}.
	 * @see org.springframework.context.ApplicationContext#getEnvironment()
	 * @see org.springframework.core.env.Environment
	 * @see org.springframework.core.env.StandardEnvironment
	 * @see #getApplicationContext()
	 */
	protected Environment getEnvironment() {

		return Optional.ofNullable(getApplicationContext())
			.map(ConfigurableApplicationContext::getEnvironment)
			.orElse(new StandardEnvironment());
	}

	/**
	 * Returns a collection of {@link TypeFilter TypeFilters} used to exclude types found
	 * during the classpath component scan.
	 *
	 * @return a collection of {@link TypeFilter} objects
	 * @see org.springframework.core.type.filter.TypeFilter
	 * @see java.lang.Iterable
	 */
	protected Iterable<TypeFilter> getExcludes() {
		return this.excludes;
	}

	/**
	 * Returns a collection of {@link TypeFilter TypeFilters} used to include (match) types found
	 * during the classpath component scan.
	 *
	 * @return a collection of {@link TypeFilter} objects
	 * @see org.springframework.core.type.filter.TypeFilter
	 * @see java.lang.Iterable
	 */
	protected Iterable<TypeFilter> getIncludes() {
		return this.includes;
	}

	@Override
	public Iterator<String> iterator() {
		return getBasePackages().iterator();
	}

	/**
	 * Scans the {@link Set} of base packages searching for GemFire application components
	 * accepted by the filters of this scanner.
	 *
	 * @return a {@link Set} of GemFire application component {@link Class} types found on the classpath.
	 * @see #newClassPathScanningCandidateComponentProvider(boolean)
	 * @see java.util.Set
	 */
	public Set<Class<?>> scan() {

		Set<Class<?>> componentClasses = new CopyOnWriteArraySet<>();

		ClassLoader entityClassLoader = getEntityClassLoader();

		ClassPathScanningCandidateComponentProvider componentProvider =
			newClassPathScanningCandidateComponentProvider();

		stream(this.spliterator(), true)
			.flatMap(packageName -> componentProvider.findCandidateComponents(packageName).stream())
			.forEach(beanDefinition -> {
				Optional.ofNullable(beanDefinition.getBeanClassName())
					.filter(StringUtils::hasText)
					.ifPresent(beanClassName -> {
						try {
							componentClasses.add(ClassUtils.forName(beanClassName, entityClassLoader));
						}
						catch (ClassNotFoundException ignore) {
							log.warn(String.format("Class for component type [%s] not found",
								beanDefinition.getBeanClassName()));
						}
					});
			});

		return componentClasses;
	}

	/**
	 * Constructs a new instance of the {@link ClassPathScanningCandidateComponentProvider} initialized with
	 * no default filters.
	 *
	 * @return a new instance of the {@link ClassPathScanningCandidateComponentProvider}.
	 * @see org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider
	 * @see #newClassPathScanningCandidateComponentProvider(boolean)
	 */
	protected ClassPathScanningCandidateComponentProvider newClassPathScanningCandidateComponentProvider() {
		return newClassPathScanningCandidateComponentProvider(false);
	}

	/**
	 * Constructs a new instance of the {@link ClassPathScanningCandidateComponentProvider} initialized with
	 * the {@code useDefaultFilters} boolean value to indicate whether to use default values or not.  Additionally,
	 * the exclude/include filters are also set.
	 *
	 * @param useDefaultFilters boolean value to indicate whether to use default filters.
	 * @return a new instance of the {@link ClassPathScanningCandidateComponentProvider}.
	 * @see org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider
	 * @see #newClassPathScanningCandidateComponentProvider(boolean)
	 */
	protected ClassPathScanningCandidateComponentProvider newClassPathScanningCandidateComponentProvider(
			boolean useDefaultFilters) {

		ClassPathScanningCandidateComponentProvider componentProvider =
			new ClassPathScanningCandidateComponentProvider(useDefaultFilters, getEnvironment());

		this.excludes.forEach(componentProvider::addExcludeFilter);
		this.includes.forEach(componentProvider::addIncludeFilter);

		return componentProvider;
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner with(ClassLoader entityClassLoader) {
		this.entityClassLoader = entityClassLoader;
		return this;
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner with(ConfigurableApplicationContext applicationContext) {
		this.applicationContext = applicationContext;
		return this;
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withExcludes(TypeFilter... excludes) {
		return withExcludes(asSet(nullSafeArray(excludes, TypeFilter.class)));
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withExcludes(Iterable<TypeFilter> excludes) {
		stream(nullSafeIterable(excludes).spliterator(), false).forEach(this.excludes::add);
		return this;
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withIncludes(TypeFilter... includes) {
		return withIncludes(asSet(nullSafeArray(includes, TypeFilter.class)));
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withIncludes(Iterable<TypeFilter> includes) {
		stream(nullSafeIterable(includes).spliterator(), false).forEach(this.includes::add);
		return this;
	}
}
