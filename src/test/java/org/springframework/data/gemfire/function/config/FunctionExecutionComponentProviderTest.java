/*
 * Copyright 2002-2019 the original author or authors.
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

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.junit.Test;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.ScannedGenericBeanDefinition;
import org.springframework.core.type.filter.AssignableTypeFilter;
import org.springframework.core.type.filter.TypeFilter;
import org.springframework.data.gemfire.function.config.one.TestFunctionExecution;

/**
 * @author David Turanski
 *
 */
public class FunctionExecutionComponentProviderTest {

	@Test
	public void testDiscovery() throws ClassNotFoundException {
		List<TypeFilter> includeFilters = new ArrayList<TypeFilter>();
		FunctionExecutionComponentProvider provider = new FunctionExecutionComponentProvider(includeFilters,
				AnnotationFunctionExecutionConfigurationSource.getFunctionExecutionAnnotationTypes());
		Set<BeanDefinition> candidates = provider.findCandidateComponents(this.getClass().getPackage().getName()
				+ ".one");

		ScannedGenericBeanDefinition bd = null;

		for (BeanDefinition candidate : candidates) {
			if (candidate.getBeanClassName().equals(TestFunctionExecution.class.getName())) {
				bd = (ScannedGenericBeanDefinition) candidate;
			}
		}

		assertNotNull(bd);

	}

	@Test
	public void testExcludeFilter() throws ClassNotFoundException {
		List<TypeFilter> includeFilters = new ArrayList<TypeFilter>();
		FunctionExecutionComponentProvider provider = new FunctionExecutionComponentProvider(includeFilters,
				AnnotationFunctionExecutionConfigurationSource.getFunctionExecutionAnnotationTypes());

		provider.addExcludeFilter(new AssignableTypeFilter(TestFunctionExecution.class));

		Set<BeanDefinition> candidates = provider.findCandidateComponents(this.getClass().getPackage().getName()
				+ ".one");

		for (BeanDefinition candidate : candidates) {
			if (candidate.getBeanClassName().equals(TestFunctionExecution.class.getName())) {
				fail(TestFunctionExecution.class.getName() + " not excluded");
			}
		}
	}

}
