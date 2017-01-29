/*
 * Copyright 2002-2018 the original author or authors.
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
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

/**
 * Annotation based configuration source for function executions
 *
 * @author David Turanski
 *
 */
class AnnotationFunctionExecutionConfigurationSource extends AbstractFunctionExecutionConfigurationSource {

	private static final String BASE_PACKAGES = "basePackages";
	private static final String BASE_PACKAGE_CLASSES = "basePackageClasses";

	private final AnnotationMetadata metadata;
	private final AnnotationAttributes attributes;


	/**
	 * Creates a new {@link AnnotationFunctionExecutionConfigurationSource} from the given {@link AnnotationMetadata} and
	 * annotation.
	 *
	 * @param metadata must not be {@literal null}.
	 */
	 AnnotationFunctionExecutionConfigurationSource(AnnotationMetadata metadata) {

		Assert.notNull(metadata);

		this.attributes = new AnnotationAttributes(metadata.getAnnotationAttributes(EnableGemfireFunctionExecutions.class.getName()));
		this.metadata = metadata;

	}



	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.FunctionExecutionConfigurationSource#getSource()
	 */
	@Override
	public Object getSource() {
		// TODO Auto-generated method stub
		return this.metadata;
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.config.FunctionExecutionConfigurationSource#getBasePackages()
	 */
	@Override
	public Iterable<String> getBasePackages() {
		String[] value = attributes.getStringArray("value");
		String[] basePackages = attributes.getStringArray(BASE_PACKAGES);
		Class<?>[] basePackageClasses = attributes.getClassArray(BASE_PACKAGE_CLASSES);

		// Default configuration - return package of annotated class
		if (value.length == 0 && basePackages.length == 0 && basePackageClasses.length == 0) {
			String className = metadata.getClassName();
			return Collections.singleton(className.substring(0, className.lastIndexOf('.')));
		}

		Set<String> packages = new HashSet<String>();
		packages.addAll(Arrays.asList(value));
		packages.addAll(Arrays.asList(basePackages));

		for (Class<?> typeName : basePackageClasses) {
			packages.add(ClassUtils.getPackageName(typeName));
		}

		return packages;
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
