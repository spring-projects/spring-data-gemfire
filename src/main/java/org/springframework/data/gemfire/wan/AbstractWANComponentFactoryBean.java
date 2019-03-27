/*
 * Copyright 2010-2019 the original author or authors.
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
 * Abstract base class for WAN Gateway components.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.Cache
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 */
public abstract class AbstractWANComponentFactoryBean<T>
		implements BeanNameAware, DisposableBean, FactoryBean<T>, InitializingBean {

	protected final Cache cache;

	protected final Log log = LogFactory.getLog(getClass());

	protected Object factory;

	private String beanName;
	private String name;

	protected AbstractWANComponentFactoryBean(Cache cache) {
		this.cache = cache;
	}

	@Override
	public void setBeanName(String beanName) {
		this.beanName = beanName;
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
	public final boolean isSingleton() {
		return true;
	}

	@Override
	public final void afterPropertiesSet() throws Exception {

		Assert.notNull(this.cache, "Cache must not be null");
		Assert.notNull(getName(), "Name must not be null");

		doInit();
	}

	protected abstract void doInit() throws Exception;

	@Override
	public void destroy() throws Exception { }

}
