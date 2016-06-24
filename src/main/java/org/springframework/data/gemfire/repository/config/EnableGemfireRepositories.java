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
 */
package org.springframework.data.gemfire.repository.config;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.gemfire.repository.support.SimpleGemfireRepository;
import org.springframework.data.repository.query.QueryLookupStrategy;
import org.springframework.data.repository.query.QueryLookupStrategy.Key;

/**
 * Annotation to enable Gemfire repositories.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Inherited
@Import(GemfireRepositoriesRegistrar.class)
public @interface EnableGemfireRepositories {

	/**
	 * Alias for the {@link #basePackages()} attribute. Allows for more concise annotation declarations, e.g.
	 * {@code @EnableGemfireRepositories("org.my.pkg")} instead of
	 * {@code @EnableGemfireRepositories(basePackages="org.my.pkg")}.
	 *
	 * @return a String array specifying the packages to search for GemFire Repositories.
	 * @see #basePackages()
	 */
	String[] value() default {};

	/**
	 * Base packages to scan for annotated components. {@link #value()} is an alias for (and mutually exclusive with) this
	 * attribute. Use {@link #basePackageClasses()} for a type-safe alternative to String-based package names.
	 *
	 * @return a String array specifying the packages to search for GemFire Repositories.
	 * @see #value()
	 */
	String[] basePackages() default {};

	/**
	 * Type-safe alternative to {@link #basePackages()} for specifying the packages to scan for annotated components. The
	 * package of each class specified will be scanned. Consider creating a special no-op marker class or interface in
	 * each package that serves no purpose other than being referenced by this attribute.
	 *
	 * @return an array of classes used to determine the packages to scan for GemFire Repositories.
	 */
	Class<?>[] basePackageClasses() default {};

	/**
	 * Specifies which types are eligible for component scanning. Further narrows the set of candidate components from
	 * everything in {@link #basePackages()} to everything in the base packages that matches the given filter or filters.
	 *
	 * @return an array of Filters used to specify Repositories to be included during the component scan.
	 */
	Filter[] includeFilters() default {};

	/**
	 * Specifies which types are not eligible for component scanning.
	 *
	 * @return an array of Filters used to specify Repositories to be excluded during the component scan.
	 */
	Filter[] excludeFilters() default {};

	/**
	 * Returns the postfix to be used when looking up custom repository implementations. Defaults to {@literal Impl}. So
	 * for a repository named {@code PersonRepository} the corresponding implementation class will be looked up scanning
	 * for {@code PersonRepositoryImpl}.
	 *
	 * @return a String indicating the postfix to append to the Repository interface name when looking up the custom
	 * Repository implementing class.
	 */
	String repositoryImplementationPostfix() default "Impl";

	/**
	 * Configures the location of where to find the Spring Data named queries properties file. Will default to
	 * {@code META-INFO/jpa-named-queries.properties}.
	 *
	 * @return a String indicating the location of the name queries properties file.
	 */
	String namedQueriesLocation() default "";

	/**
	 * Returns the key of the {@link QueryLookupStrategy} to be used for lookup queries for query methods. Defaults to
	 * {@link Key#CREATE_IF_NOT_FOUND}.
	 *
	 * @return the Key used to determine the Query lookup and creation strategy.
	 */
	Key queryLookupStrategy() default Key.CREATE_IF_NOT_FOUND;

	/**
	 * Returns the {@link FactoryBean} class to be used for each repository instance. Defaults to
	 * {@link GemfireRepositoryFactoryBean}.
	 *
	 * @return the {@link FactoryBean} class type used for each Repository interface.
	 */
	Class<?> repositoryFactoryBeanClass() default GemfireRepositoryFactoryBean.class;

	/**
	 * Configure the repository base class to be used to create repository proxies for this particular configuration.
	 *
	 * @since 1.7
	 */
	Class<?> repositoryBaseClass() default SimpleGemfireRepository.class;

	/**
	 * Configures the name of the {@link GemfireMappingContext} bean definition to be used to create repositories
	 * discovered through this annotation. If not configured a default one will be created.
	 *
	 * @return the bean name of the {@link org.springframework.data.mapping.context.MappingContext} used by the
	 * Repository to map entities to the underlying data store.
	 */
	String mappingContextRef() default "";

}
