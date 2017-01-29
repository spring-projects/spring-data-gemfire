/*
 * Copyright 2010-2018 the original author or authors.
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

import java.util.Arrays;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.Cache;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Base class for GemFire WAN Gateway component factory beans.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 */
public abstract class AbstractWANComponentFactoryBean<T> implements BeanNameAware, FactoryBean<T>,
		InitializingBean, DisposableBean {

	protected static final List<String> VALID_ORDER_POLICIES = Arrays.asList("KEY", "PARTITION", "THREAD");

	protected Log log = LogFactory.getLog(getClass());

	protected final Cache cache;

	protected Object factory;

	private String beanName;
	private String name;

	protected AbstractWANComponentFactoryBean(final Cache cache) {
		this.cache = cache;
	}

	@Override
	public final void setBeanName(final String beanName) {
		this.beanName = beanName;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public void setName(final String name) {
		this.name = name;
	}

	public String getName() {
		return (StringUtils.hasText(name) ? name : beanName);
	}

	@Override
	public abstract T getObject() throws Exception;

	@Override
	public abstract Class<?> getObjectType();

	@Override
	public final boolean isSingleton() {
		return true;
	}

	@Override
	public final void afterPropertiesSet() throws Exception {
		Assert.notNull(cache, "Cache must not be null.");
		Assert.notNull(getName(), "Name must not be null.");
		doInit();
	}

	protected abstract void doInit() throws Exception;

	@Override
	public void destroy() throws Exception {
	}

}
