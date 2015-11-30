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
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Declarable;

/**
 * Dedicated {@link Declarable} support class for wiring the declaring instance through the Spring container.
 *
 * <p>This implementation first looks for a 'bean-name' property which will be used to locate a 'template'
 * bean definition.  Autowiring will be performed, based on the settings defined in the Spring container.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.wiring.BeanConfigurerSupport
 * @see org.springframework.beans.factory.wiring.BeanWiringInfo
 * @see org.springframework.data.gemfire.DeclarableSupport
 * @see org.springframework.data.gemfire.LazyWiringDeclarableSupport
 * @see com.gemstone.gemfire.cache.Declarable
 * @deprecated please use LazyWiringDeclarableSupport instead.
 */
@Deprecated
public class WiringDeclarableSupport extends DeclarableSupport {

	private static final String BEAN_NAME_PROPERTY = "bean-name";

	@Override
	protected void initInstance(Properties parameters) {
		BeanFactory beanFactory = getBeanFactory();

		BeanConfigurerSupport beanConfigurer = new BeanConfigurerSupport();
		beanConfigurer.setBeanFactory(beanFactory);

		final String beanName = parameters.getProperty(BEAN_NAME_PROPERTY);

		if (StringUtils.hasText(beanName)) {
			if (!beanFactory.containsBean(beanName)) {
				throw new IllegalArgumentException(String.format("Cannot find bean named '%1$s'", beanName));
			}

			beanConfigurer.setBeanWiringInfoResolver(new BeanWiringInfoResolver() {
				public BeanWiringInfo resolveWiringInfo(Object beanInstance) {
					return new BeanWiringInfo(beanName);
				}
			});
		}

		beanConfigurer.afterPropertiesSet();
		beanConfigurer.configureBean(this);
		beanConfigurer.destroy();
	}

}
