/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.wan;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Abstract base class for WAN Gateway objects.
 *
 * @author David Turanski
 * @author John Blum
 * @author Udo Kohlmeyer
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 */
public abstract class AbstractWANComponentFactoryBean<T> extends AbstractFactoryBeanSupport<T>
		implements DisposableBean, InitializingBean {

	@Autowired
	protected Cache cache;

	protected final Logger logger = LoggerFactory.getLogger(getClass());

	protected Object factory;

	private String beanName;
	private String name;

	protected AbstractWANComponentFactoryBean() { }

	protected AbstractWANComponentFactoryBean(GemFireCache cache) {
		this.cache = (Cache) cache;
	}

	@Override
	public void setBeanName(String beanName) {
		this.beanName = beanName;
	}

	public Cache getCache() {
		return this.cache;
	}

	public void setCache(Cache cache) {
		this.cache = cache;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		return StringUtils.hasText(this.name) ? this.name : this.beanName;
	}

	@Override
	public final void afterPropertiesSet() throws Exception {

		Assert.notNull(getCache(), "Cache must not be null");
		Assert.notNull(getName(), "Name must not be null");

		doInit();
	}

	protected abstract void doInit() throws Exception;

	@Override
	public void destroy() { }

}
