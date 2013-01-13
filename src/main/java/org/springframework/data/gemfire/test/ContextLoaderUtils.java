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
package org.springframework.data.gemfire.test;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.util.StringUtils;

/**
 * @author David Turanski
 *
 */
class ContextLoaderUtils {
	/**
	 * 
	 */
	static private Log logger = LogFactory.getLog(ContextLoaderUtils.class);
	static void customizeBeanFactory(DefaultListableBeanFactory beanFactory) {
		if (StringUtils.hasText(System.getProperty(GemfireTestRunner.GEMFIRE_TEST_RUNNER_DISABLED))) {
			String value = System.getProperty(GemfireTestRunner.GEMFIRE_TEST_RUNNER_DISABLED);
			if (!(value.equalsIgnoreCase("NO") || value.equalsIgnoreCase("FALSE"))) {
				logger.warn("Mocks disabled using real GemFire components:" + GemfireTestRunner.GEMFIRE_TEST_RUNNER_DISABLED + " = " + System.getProperty(GemfireTestRunner.GEMFIRE_TEST_RUNNER_DISABLED));
				return;
			}
		}
		beanFactory.addBeanPostProcessor(new GemfireTestBeanPostProcessor());
	}
}
