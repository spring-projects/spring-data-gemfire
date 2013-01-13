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
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.test.context.TestContext;
import org.springframework.test.context.TestExecutionListener;

import com.gemstone.gemfire.cache.Cache;

/**
 * @author David Turanski
 *
 */
public class GemfireTestExecutionListener implements TestExecutionListener {
	private static Log logger = LogFactory.getLog(GemfireTestExecutionListener.class);

	/* (non-Javadoc)
	 * @see org.springframework.test.context.TestExecutionListener#beforeTestClass(org.springframework.test.context.TestContext)
	 */
	@Override
	public void beforeTestClass(TestContext testContext) throws Exception {
		 
	}

	/* (non-Javadoc)
	 * @see org.springframework.test.context.TestExecutionListener#prepareTestInstance(org.springframework.test.context.TestContext)
	 */
	@Override
	public void prepareTestInstance(TestContext testContext) throws Exception {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.springframework.test.context.TestExecutionListener#beforeTestMethod(org.springframework.test.context.TestContext)
	 */
	@Override
	public void beforeTestMethod(TestContext testContext) throws Exception {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.springframework.test.context.TestExecutionListener#afterTestMethod(org.springframework.test.context.TestContext)
	 */
	@Override
	public void afterTestMethod(TestContext testContext) throws Exception {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.springframework.test.context.TestExecutionListener#afterTestClass(org.springframework.test.context.TestContext)
	 */
	@Override
	public void afterTestClass(TestContext testContext) throws Exception {
		// TODO Auto-generated method stub
		
	}
	
	public static class GemfireTestBeanPostProcessor implements BeanPostProcessor {

		/* (non-Javadoc)
		 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessBeforeInitialization(java.lang.Object, java.lang.String)
		 */
		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
			logger.debug("I have bean " + beanName);
			return bean;
		}

		/* (non-Javadoc)
		 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessAfterInitialization(java.lang.Object, java.lang.String)
		 */
		@Override
		public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
			// TODO Auto-generated method stub
			return null;
		}
		
	}
}
