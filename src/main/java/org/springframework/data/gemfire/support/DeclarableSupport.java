/*
 * Copyright 2016-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.support;

import static org.springframework.data.gemfire.support.GemfireBeanFactoryLocator.newBeanFactoryLocator;

import org.apache.geode.cache.CacheCallback;
import org.apache.geode.cache.Declarable;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.RegionFactoryBean;

/**
 * Convenience class for Spring-aware GemFire {@link Declarable} components.  Provides subclasses with a reference
 * to the current Spring {@link BeanFactory} in orde to perform Spring bean lookups or resource loading.
 *
 * Note, in most cases, the developer should just declare the same components as Spring beans in the Spring container,
 * through {@link RegionFactoryBean}, which gives access to the full Spring container capabilities and does not
 * enforce the {@link Declarable} interface to be implemented.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
 * @see org.apache.geode.cache.CacheCallback
 * @see org.apache.geode.cache.Declarable
 */
@SuppressWarnings("unused")
public abstract class DeclarableSupport implements CacheCallback, Declarable {

	private String beanFactoryKey = null;

	/**
	 * Returns a reference to the Spring {@link BeanFactory}.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator#useBeanFactory(String)
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #locateBeanFactory()
	 */
	protected BeanFactory getBeanFactory() {
		return locateBeanFactory();
	}

	/**
	 * Set the key used to lookup the Spring {@link BeanFactory}.
	 *
	 * @param beanFactoryKey {@link String} containing the key used to lookup the Spring {@link BeanFactory}.
	 */
	public void setBeanFactoryKey(String beanFactoryKey) {
		this.beanFactoryKey = beanFactoryKey;
	}

	/**
	 * Returns the key used to lookup the Spring {@link BeanFactory}.
	 *
	 * @return a {@link String} containing the key used to lookup the Spring {@link BeanFactory}.
	 */
	protected String getBeanFactoryKey() {
		return this.beanFactoryKey;
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory}.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #locateBeanFactory(String)
	 * @see #getBeanFactoryKey()
	 */
	protected BeanFactory locateBeanFactory() {
		return locateBeanFactory(getBeanFactoryKey());
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory} for the given {@code beanFactoryKey}.
	 *
	 * @param beanFactoryKey {@link String} containing the key used to lookup the Spring {@link BeanFactory}.
	 * @return a reference to the Spring {@link BeanFactory} for the given {@code beanFactoryKey}.
	 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator#useBeanFactory(String)
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory locateBeanFactory(String beanFactoryKey) {
		return newBeanFactoryLocator().useBeanFactory(beanFactoryKey);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void close() {
	}
}
