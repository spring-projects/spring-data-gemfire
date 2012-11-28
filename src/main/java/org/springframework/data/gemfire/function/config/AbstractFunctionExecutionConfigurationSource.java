/*
 * Copyright 2002-2012 the original author or authors.
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
import java.util.HashSet;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ClassPathScanningCandidateComponentProvider;
import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.core.io.ResourceLoader;
import org.springframework.core.type.filter.TypeFilter;

/**
 * Annotation based configuration source for function executions
 * 
 * @author David Turanski
 *
 */
abstract class AbstractFunctionExecutionConfigurationSource implements FunctionExecutionConfigurationSource {
	protected Log logger = LogFactory.getLog(this.getClass());
		 
	 
	private static Set<Class<? extends Annotation>> functionExecutionAnnotationTypes;
	
	static {
	  functionExecutionAnnotationTypes = new HashSet<Class<? extends Annotation>>();
	  functionExecutionAnnotationTypes.add(OnRegion.class);
	  functionExecutionAnnotationTypes.add(OnServer.class);
	  functionExecutionAnnotationTypes.add(OnServers.class);
	  functionExecutionAnnotationTypes.add(OnMember.class);
	  functionExecutionAnnotationTypes.add(OnMembers.class);
	}
	

	static Set<Class<? extends Annotation>> getFunctionExecutionAnnotationTypes() {
		return functionExecutionAnnotationTypes;
	}
	
	
	public Collection<ScannedGenericBeanDefinition> getCandidates(ResourceLoader loader) {
		ClassPathScanningCandidateComponentProvider scanner = new FunctionExecutionComponentProvider(getIncludeFilters(),functionExecutionAnnotationTypes);
		scanner.setResourceLoader(loader);

		for (TypeFilter filter : getExcludeFilters()) {
			scanner.addExcludeFilter(filter);
		}

		Set<ScannedGenericBeanDefinition> result = new HashSet<ScannedGenericBeanDefinition>();

		for (String basePackage : getBasePackages()) {
			if (logger.isDebugEnabled()) {
				logger.debug("scanning package " + basePackage);
			}
			Collection<BeanDefinition> components = scanner.findCandidateComponents(basePackage);
			for (BeanDefinition definition : components) {
				result.add((ScannedGenericBeanDefinition)definition);
			}
		}

		return result;
	}
	
}
