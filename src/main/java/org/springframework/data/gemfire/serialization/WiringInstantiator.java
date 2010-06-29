/*
 * Copyright 2010 the original author or authors.
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

package org.springframework.data.gemfire.serialization;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.wiring.BeanConfigurerSupport;

import com.gemstone.gemfire.DataSerializable;
import com.gemstone.gemfire.Instantiator;

/**
 * Instantiator that performs instance wiring using the Spring IoC container, allowing common properties
 * to be injected before the object is hydrated/deserialized. The newly created instances can be configured
 * either by relying on an existing bean definition (which acts as a template) or by providing an embedded
 * configuration through annotations.
 * 
 * <p/>
 * Can reuse existing instantiators to optimize instance creation. If one is not provided, it will fallback
 * to reflection invocation. 
 * 
 * @see org.springframework.beans.factory.wiring.BeanConfigurerSupport
 * @see org.springframework.beans.factory.wiring.BeanWiringInfoResolver
 * @see org.springframework.beans.factory.annotation.Autowired
 * @see javax.annotation.Resource
 * @see javax.inject.Inject
 * 
 * @author Costin Leau
 */
public class WiringInstantiator extends Instantiator implements BeanFactoryAware, InitializingBean,
		DisposableBean {

	private final Instantiator instantiator;
	private final Class<? extends DataSerializable> clazz;
	private BeanConfigurerSupport configurer;
	private BeanFactory beanFactory;

	public WiringInstantiator(Instantiator instantiator) {
		super(instantiator.getInstantiatedClass(), instantiator.getId());
		this.instantiator = instantiator;
		this.clazz = null;
	}

	public WiringInstantiator(Class<? extends DataSerializable> c, int classId) {
		super(c, classId);
		instantiator = null;
		clazz = c;
	}


	public void afterPropertiesSet() {
		if (configurer == null) {
			configurer = new BeanConfigurerSupport();
			configurer.setBeanFactory(beanFactory);
			configurer.afterPropertiesSet();
		}
	}

	public void destroy() throws Exception {
		configurer.destroy();
	}

	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}


	@Override
	public DataSerializable newInstance() {
		DataSerializable instance = createInstance();
		configurer.configureBean(instance);
		return instance;
	}

	private DataSerializable createInstance() {
		if (instantiator != null) {
			return instantiator.newInstance();
		}

		return BeanUtils.instantiate(clazz);
	}

	/**
	 * Sets the manager responsible for configuring the newly created instances.
	 * The given configurer needs to be configured and initialized before-hand. 
	 * 
	 * @param configurer the configurer to set
	 */
	public void setConfigurer(BeanConfigurerSupport configurer) {
		this.configurer = configurer;
	}
}