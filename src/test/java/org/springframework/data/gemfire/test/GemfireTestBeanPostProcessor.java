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
package org.springframework.data.gemfire.test;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;

/**
 * @author David Turanski
 * @author John Blum
 */
public class GemfireTestBeanPostProcessor implements BeanPostProcessor {

	private static Log logger = LogFactory.getLog(GemfireTestBeanPostProcessor.class);

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessBeforeInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		if (bean instanceof CacheFactoryBean) {
			String beanTypeName = bean.getClass().getName();

			bean = (bean instanceof ClientCacheFactoryBean
				? new MockClientCacheFactoryBean((ClientCacheFactoryBean) bean)
				: new MockCacheFactoryBean((CacheFactoryBean) bean));

			logger.info(String.format("Replacing the [%1$s] bean definition having type [%2$s] with mock [%3$s]...",
				beanName, beanTypeName, bean.getClass().getName()));
		}
		else if (bean instanceof CacheServerFactoryBean) {
			((CacheServerFactoryBean) bean).setCache(new StubCache());
		}

		return bean;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.beans.factory.config.BeanPostProcessor#postProcessAfterInitialization(java.lang.Object, java.lang.String)
	 */
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
		return bean;
	}

}
