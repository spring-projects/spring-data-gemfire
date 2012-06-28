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

package org.springframework.data.gemfire;

import java.util.Properties;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.access.BeanFactoryReference;

import com.gemstone.gemfire.cache.CacheCallback;
import com.gemstone.gemfire.cache.Declarable;

/**
 * Convenience class for Spring-aware GemFire Declarable components. Provides a
 * reference to the current Spring application context, e.g. for bean lookup or
 * resource loading.
 * 
 * Note that in most cases, one can just declare the same components as Spring
 * beans, through {@link RegionFactoryBean} which gives access to the full
 * container capabilities and does not enforce the {@link Declarable} interface
 * to be implemented.
 * 
 * @author Costin Leau
 */
public abstract class DeclarableSupport implements CacheCallback, Declarable {

	private String factoryKey = null;

	private BeanFactoryReference bfReference = null;

	public DeclarableSupport() {
	}

	/**
	 * This implementation uses this method as a lifecycle hook to initialize
	 * the bean factory locator.
	 * 
	 * {@inheritDoc}
	 * 
	 * @see #setFactoryKey(String)
	 */
	@Override
	public final void init(Properties props) {
		bfReference = new GemfireBeanFactoryLocator().useBeanFactory(factoryKey);
		initInstance(props);
	}

	/**
	 * Initialize the current instance based on the given properties.
	 * 
	 * @param props
	 */
	protected void initInstance(Properties props) {
	}

	protected BeanFactory getBeanFactory() {
		return bfReference.getFactory();
	}

	@Override
	public void close() {
		bfReference.release();
		bfReference = null;
	}

	/**
	 * Sets the key under which the enclosing beanFactory can be found. Needed
	 * only if multiple beanFactories are used with GemFire inside the same
	 * class loader / class space.
	 * 
	 * @see GemfireBeanFactoryLocator
	 * @param key
	 */
	public void setFactoryKey(String key) {
		this.factoryKey = key;
	}
}