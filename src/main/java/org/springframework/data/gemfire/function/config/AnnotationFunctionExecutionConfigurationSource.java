/*
 * Copyright 2002-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.config;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.BeanUtils;
import org.springframework.context.annotation.FilterType;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.core.type.filter.AnnotationTypeFilter;
import org.springframework.core.type.filter.AssignableTypeFilter;
import org.springframework.core.type.filter.TypeFilter;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

/**
 * Annotation based configuration source for function executions
 *
 * @author David Turanski
 *
 */
public class AnnotationFunctionExecutionConfigurationSource extends AbstractFunctionExecutionConfigurationSource {

	private static final String BASE_PACKAGES = "basePackages";
	private static final String BASE_PACKAGE_CLASSES = "basePackageClasses";
	private static final String VALUE = "value";

	private final AnnotationMetadata metadata;
	private final AnnotationAttributes attributes;


	/**
	 * Creates a new {@link AnnotationFunctionExecutionConfigurationSource} from the given {@link AnnotationMetadata} and
	 * annotation.
	 *
	 * @param metadata must not be {@literal null}.
	 */
	 public AnnotationFunctionExecutionConfigurationSource(AnnotationMetadata metadata) {

		Assert.notNull(metadata, "AnnotationMetadata must not be null");

		this.attributes = AnnotationAttributes.fromMap(
			metadata.getAnnotationAttributes(EnableGemfireFunctionExecutions.class.getName()));

		this.metadata = metadata;
	}



	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.FunctionExecutionConfigurationSource#getSource()
	 */
	@Override
	public Object getSource() {
		return this.metadata;
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.FunctionExecutionConfigurationSource#getBasePackages()
	 */
	@Override
	public Iterable<String> getBasePackages() {

		String[] value = this.attributes.getStringArray(VALUE);
		String[] basePackages = this.attributes.getStringArray(BASE_PACKAGES);

		Class<?>[] basePackageClasses = this.attributes.getClassArray(BASE_PACKAGE_CLASSES);

		// Default configuration - return package of annotated class
		if (areAllEmpty(value, basePackages, basePackageClasses)) {

			String className = this.metadata.getClassName();

			return Collections.singleton(className.substring(0, className.lastIndexOf('.')));
		}

		Set<String> packages = new HashSet<>();

		packages.addAll(Arrays.asList(value));
		packages.addAll(Arrays.asList(basePackages));

		Arrays.stream(nullSafeArray(basePackageClasses, Class.class))
			.map(ClassUtils::getPackageName)
			.forEach(packages::add);

		return packages;
	}

	private boolean areAllEmpty(Object[]... arrays) {

		for (Object[] array : arrays) {
			if (!ArrayUtils.isEmpty(array)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public Iterable<TypeFilter> getIncludeFilters() {
		return parseFilters("includeFilters");
	}

 	@Override
	public Iterable<TypeFilter> getExcludeFilters() {
		return parseFilters("excludeFilters");
	}

	private Set<TypeFilter> parseFilters(String attributeName) {

		Set<TypeFilter> result = new HashSet<TypeFilter>();
		AnnotationAttributes[] filters = attributes.getAnnotationArray(attributeName);

		for (AnnotationAttributes filter : filters) {
			result.addAll(typeFiltersFor(filter));
		}

		return result;
	}

	/**
	 * Copy of {@code ComponentScanAnnotationParser#typeFiltersFor}.
	 *
	 * @param filterAttributes
	 * @return
	 */
	private List<TypeFilter> typeFiltersFor(AnnotationAttributes filterAttributes) {
		List<TypeFilter> typeFilters = new ArrayList<TypeFilter>();
		FilterType filterType = filterAttributes.getEnum("type");

		for (Class<?> filterClass : filterAttributes.getClassArray("value")) {
			switch (filterType) {
			case ANNOTATION:
				Assert.isAssignable(Annotation.class, filterClass, "An error occured when processing a @ComponentScan "
						+ "ANNOTATION type filter: ");
				@SuppressWarnings("unchecked")
				Class<Annotation> annoClass = (Class<Annotation>) filterClass;
				typeFilters.add(new AnnotationTypeFilter(annoClass));
				break;
			case ASSIGNABLE_TYPE:
				typeFilters.add(new AssignableTypeFilter(filterClass));
				break;
			case CUSTOM:
				Assert.isAssignable(TypeFilter.class, filterClass, "An error occured when processing a @ComponentScan "
						+ "CUSTOM type filter: ");
				typeFilters.add(BeanUtils.instantiateClass(filterClass, TypeFilter.class));
				break;
			default:
				throw new IllegalArgumentException("unknown filter type " + filterType);
			}
		}
		return typeFilters;
	}

}
