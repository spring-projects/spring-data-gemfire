/*
 * Copyright 2010-2019 the original author or authors.
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
import org.springframework.beans.factory.access.BeanFactoryReference;

import com.gemstone.gemfire.cache.CacheCallback;
import com.gemstone.gemfire.cache.Declarable;

/**
 * Convenience class for Spring-aware GemFire Declarable components. Provides a reference to the current
 * Spring ApplicationContext, e.g. for Spring bean lookup or resource loading.
 * 
 * Note that in most cases, one can just declare the same components as Spring beans, through {@link RegionFactoryBean}
 * which gives access to the full Spring container capabilities and does not enforce the {@link Declarable} interface
 * to be implemented.
 * 
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.access.BeanFactoryReference
 * @see com.gemstone.gemfire.cache.CacheCallback
 * @see com.gemstone.gemfire.cache.Declarable
 */
@SuppressWarnings("unused")
public abstract class DeclarableSupport implements CacheCallback, Declarable {

	private String beanFactoryKey = null;

	private BeanFactoryReference beanFactoryReference = null;

	public DeclarableSupport() {
	}

	/**
	 * Gets a reference to the configured Spring BeanFactory.
	 *
	 * @return a Spring BeanFactory reference.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory getBeanFactory() {
		return beanFactoryReference.getFactory();
	}

	/**
	 * Sets the key under which the enclosing BeanFactory can be found. Needed only if multiple BeanFactories
	 * are used with GemFire inside the same class loader / class space.
	 *
	 * @param key a String specifying the key used to lookup the "enclosing" BeanFactory in the presence
	 * of multiple BeanFactories.
	 * @see org.springframework.data.gemfire.GemfireBeanFactoryLocator
	 */
	public void setFactoryKey(String key) {
		this.beanFactoryKey = key;
	}

	/**
	 * This Declarable implementation uses the init method as a lifecycle hook to initialize the bean factory locator.
	 *
	 * {@inheritDoc}
	 *
	 * @see org.springframework.data.gemfire.GemfireBeanFactoryLocator
	 * @see #setFactoryKey(String)
	 */
	@Override
	public final void init(Properties parameters) {
		beanFactoryReference = new GemfireBeanFactoryLocator().useBeanFactory(beanFactoryKey);
		initInstance(parameters);
	}

	/**
	 * Initialize this Declarable object with the given Properties.
	 *
	 * @param props the Properties (parameters) used to initialize this Declarable.
	 * @see com.gemstone.gemfire.cache.Declarable#init(java.util.Properties)
	 * @see java.util.Properties
	 * @see #init(Properties)
	 */
	protected void initInstance(Properties props) {
	}

	/* (non-Javadoc) */
	@Override
	public void close() {
		beanFactoryReference.release();
		beanFactoryReference = null;
	}

}
