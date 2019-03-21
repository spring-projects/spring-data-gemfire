/*
 * Copyright 2016-2019 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation.support;

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.core.env.Environment;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.core.type.filter.TypeFilter;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

/**
 * The {@link GemFireComponentClassTypeScanner} class is a classpath component scanner used to search
 * for GemFire components based on {@link Class} type.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider
 * @see org.springframework.core.env.Environment
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
		return new GemFireComponentClassTypeScanner(CollectionUtils.asSet(
			ArrayUtils.nullSafeArray(basePackages, String.class)));
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
		Set<String> basePackageSet = new HashSet<String>();

		for (String basePackage : CollectionUtils.nullSafeIterable(basePackages)) {
			basePackageSet.add(basePackage);
		}

		return new GemFireComponentClassTypeScanner(basePackageSet);
	}

	private ClassLoader entityClassLoader;

	private ConfigurableApplicationContext applicationContext;

	private Set<TypeFilter> excludes = new HashSet<TypeFilter>();
	private Set<TypeFilter> includes = new HashSet<TypeFilter>();

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
		Assert.notEmpty(basePackages, "Base packages must be specified");
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
	 */
	protected Set<String> getBasePackages() {
		return Collections.unmodifiableSet(this.basePackages);
	}

	/**
	 * Returns a reference to the {@link ClassLoader} to find and load GemFire application persistent entity classes.
	 *
	 * @return a {@link ClassLoader} to find and load GemFire application persistent entity classes.
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
		ConfigurableApplicationContext applicationContext = getApplicationContext();
		return (applicationContext != null ? applicationContext.getEnvironment() : new StandardEnvironment());
	}

	/**
	 * Returns a collection of {@link TypeFilter TypeFilters} used to exclude types found
	 * during the classpath component scan.
	 *
	 * @return a collection of {@link TypeFilter} objects
	 * @see org.springframework.core.type.filter.TypeFilter
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
	 */
	protected Iterable<TypeFilter> getIncludes() {
		return this.includes;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Iterator<String> iterator() {
		return getBasePackages().iterator();
	}

	/**
	 * Scans the {@link Set} of base packages searching for GemFire application components accepted by the filters
	 * of this scanner.
	 *
	 * @return a {@link Set} of GemFire application component {@link Class} types.
	 * @see #newClassPathScanningCandidateComponentProvider(boolean)
	 * @see java.util.Set
	 */
	public Set<Class<?>> scan() {
		Set<Class<?>> componentClasses = new HashSet<Class<?>>();

		ClassLoader entityClassLoader = getEntityClassLoader();

		ClassPathScanningCandidateComponentProvider componentProvider =
			newClassPathScanningCandidateComponentProvider();

		for (String packageName : this) {
			for (BeanDefinition beanDefinition : componentProvider.findCandidateComponents(packageName)) {
				try {
					componentClasses.add(ClassUtils.forName(beanDefinition.getBeanClassName(), entityClassLoader));
				}
				catch (ClassNotFoundException ignore) {
					log.warn(String.format("Class not found for component type [%s]",
						beanDefinition.getBeanClassName()));
				}
			}
		}

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

		for (TypeFilter exclude : excludes) {
			componentProvider.addExcludeFilter(exclude);
		}

		for (TypeFilter include : includes) {
			componentProvider.addIncludeFilter(include);
		}

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
		return withExcludes(CollectionUtils.asSet(ArrayUtils.nullSafeArray(excludes, TypeFilter.class)));
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withExcludes(Iterable<TypeFilter> excludes) {
		for (TypeFilter exclude : CollectionUtils.nullSafeIterable(excludes)) {
			this.excludes.add(exclude);
		}

		return this;
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withIncludes(TypeFilter... includes) {
		return withIncludes(CollectionUtils.asSet(ArrayUtils.nullSafeArray(includes, TypeFilter.class)));
	}

	/* (non-Javadoc) */
	public GemFireComponentClassTypeScanner withIncludes(Iterable<TypeFilter> includes) {
		for (TypeFilter include : CollectionUtils.nullSafeIterable(includes)) {
			this.includes.add(include);
		}

		return this;
	}
}
