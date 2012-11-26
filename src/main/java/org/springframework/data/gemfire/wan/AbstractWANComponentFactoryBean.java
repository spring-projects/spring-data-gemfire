/*
 * Copyright 2010-2012 the original author or authors.
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
package org.springframework.data.gemfire.wan;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;

/**
 * Base class for Gemfire WAN Gateway component factory beans
 * @author David Turanski
 * 
 */
public abstract class AbstractWANComponentFactoryBean<T> implements FactoryBean<T>, InitializingBean, BeanNameAware,
		DisposableBean {
	protected Log log = LogFactory.getLog(this.getClass());

	private String name;

	protected final Cache cache;

	protected Object factory;

	private String beanName;

	protected AbstractWANComponentFactoryBean(Cache cache) {
		this.cache = cache;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getName() {
		return name!=null ? name: beanName;
	}

	@Override
	public void destroy() throws Exception {
		// TODO Auto-generated method stub
	}

	@Override
	public final void setBeanName(String beanName) {
		this.beanName = beanName;
	}

	@Override
	public final void afterPropertiesSet() throws Exception {
		Assert.notNull(getName(), "Name cannot be null");
		Assert.notNull(cache, "Cache cannot be null");
		doInit();
	}

	protected abstract void doInit();

	@Override
	public abstract T getObject() throws Exception;

	@Override
	public abstract Class<?> getObjectType();

	@Override
	public final boolean isSingleton() {
		return true;
	}
	
	public void setFactory(Object factory) {
		this.factory = factory;
	}
	
}