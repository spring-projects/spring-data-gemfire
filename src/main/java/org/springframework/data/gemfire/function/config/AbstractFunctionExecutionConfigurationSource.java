/*
<<<<<<< Updated upstream
 * Copyright 2002-2019 the original author or authors.
=======
 * Copyright 2002-2019 the original author or authors.
>>>>>>> Stashed changes
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.config;

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.gemfire.function.annotation.OnMember;
import org.springframework.data.gemfire.function.annotation.OnMembers;
import org.springframework.data.gemfire.function.annotation.OnRegion;
import org.springframework.data.gemfire.function.annotation.OnServer;
import org.springframework.data.gemfire.function.annotation.OnServers;

/**
<<<<<<< Updated upstream
 * Annotation based configuration source for function executions
 *
 * @author David Turanski
=======
 * Abstract base class and configuration source for Function Executions.
>>>>>>> Stashed changes
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.function.config.FunctionExecutionConfiguration
 */
public abstract class AbstractFunctionExecutionConfigurationSource implements FunctionExecutionConfigurationSource {

	private static Set<Class<? extends Annotation>> functionExecutionAnnotationTypes;

	static {

		Set<Class<? extends Annotation>> annotationTypes = new HashSet<>(5);

		annotationTypes.add(OnMember.class);
		annotationTypes.add(OnMembers.class);
		annotationTypes.add(OnRegion.class);
		annotationTypes.add(OnServer.class);
		annotationTypes.add(OnServers.class);

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

		StreamSupport.stream(nullSafeIterable(getExcludeFilters()).spliterator(), false)
			.forEach(scanner::addExcludeFilter);

		Set<ScannedGenericBeanDefinition> result = new HashSet<>();

		for (String basePackage : getBasePackages()) {

			if (logger.isDebugEnabled()) {
				logger.debug("scanning package " + basePackage);
			}

			scanner.findCandidateComponents(basePackage).stream()
				.map(beanDefinition -> (ScannedGenericBeanDefinition) beanDefinition)
				.forEach(result::add);
		}

		return result;
	}
}
