/*
 * Copyright 2002-2013 the original author or authors.
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
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.type.filter.TypeFilter;
import org.springframework.data.gemfire.function.annotation.OnMember;
import org.springframework.data.gemfire.function.annotation.OnMembers;
import org.springframework.data.gemfire.function.annotation.OnRegion;
import org.springframework.data.gemfire.function.annotation.OnServer;
import org.springframework.data.gemfire.function.annotation.OnServers;

/**
 * Annotation based configuration source for function executions
 *
 * @author David Turanski
 *
 */
public abstract class AbstractFunctionExecutionConfigurationSource implements FunctionExecutionConfigurationSource {

	private static Set<Class<? extends Annotation>> functionExecutionAnnotationTypes;

	static {

		Set<Class<? extends Annotation>> annotationTypes = new HashSet<>(5);

		annotationTypes.add(OnRegion.class);
		annotationTypes.add(OnServer.class);
		annotationTypes.add(OnServers.class);
		annotationTypes.add(OnMember.class);
		annotationTypes.add(OnMembers.class);

		functionExecutionAnnotationTypes = Collections.unmodifiableSet(annotationTypes);
	}

	public static Set<Class<? extends Annotation>> getFunctionExecutionAnnotationTypes() {
		return functionExecutionAnnotationTypes;
	}

	public static Set<String> getFunctionExecutionAnnotationTypeNames() {
		return getFunctionExecutionAnnotationTypes().stream().map(Class::getName).collect(Collectors.toSet());
	}

	protected Logger logger = LoggerFactory.getLogger(getClass());

	public Collection<ScannedGenericBeanDefinition> getCandidates(ResourceLoader loader) {

		ClassPathScanningCandidateComponentProvider scanner =
			new FunctionExecutionComponentProvider(getIncludeFilters(), getFunctionExecutionAnnotationTypes());

		scanner.setResourceLoader(loader);

		for (TypeFilter filter : getExcludeFilters()) {
			scanner.addExcludeFilter(filter);
		}

		Set<ScannedGenericBeanDefinition> result = new HashSet<>();

		for (String basePackage : getBasePackages()) {

			if (logger.isDebugEnabled()) {
				logger.debug("scanning package " + basePackage);
			}

			Collection<BeanDefinition> candidateComponents = scanner.findCandidateComponents(basePackage);

			for (BeanDefinition beanDefinition : candidateComponents) {
				result.add((ScannedGenericBeanDefinition) beanDefinition);
			}
		}

		return result;
	}
}
