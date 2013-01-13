/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire;

import java.util.Properties;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.wiring.BeanConfigurerSupport;
import org.springframework.beans.factory.wiring.BeanWiringInfo;
import org.springframework.beans.factory.wiring.BeanWiringInfoResolver;

import com.gemstone.gemfire.cache.Declarable;

/**
 * Dedicated {@link Declarable} support class for wiring the declaring instance through
 * the Spring container.
 * This implementation will first look for a 'bean-name' property which will be used to
 * locate a 'template' bean definition. In case the property is not given, a bean named
 * after the class will be searched and if none is found, autowiring will be performed,
 * based on the settings defined in the Spring container.
 * 
 * @author Costin Leau
 */
public class WiringDeclarableSupport extends DeclarableSupport {

	private static final String BEAN_NAME_PROP = "bean-name";

	@Override
	protected void initInstance(Properties props) {
		BeanFactory bf = getBeanFactory();
		BeanConfigurerSupport configurer = new BeanConfigurerSupport();
		configurer.setBeanFactory(bf);

		final String beanName = props.getProperty(BEAN_NAME_PROP);
		// key specified, search for a bean
		if (beanName != null) {
			if (!bf.containsBean(beanName)) {
				throw new IllegalArgumentException("Cannot find bean named '" + beanName + "'");
			}
			configurer.setBeanWiringInfoResolver(new BeanWiringInfoResolver() {

				public BeanWiringInfo resolveWiringInfo(Object beanInstance) {
					return new BeanWiringInfo(beanName);
				}
			});
		}

		configurer.afterPropertiesSet();
		configurer.configureBean(this);
		configurer.destroy();
	}
}
