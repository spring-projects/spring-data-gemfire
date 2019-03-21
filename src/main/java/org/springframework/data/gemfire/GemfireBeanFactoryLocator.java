/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire;

import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.FatalBeanException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.access.BeanFactoryLocator;
import org.springframework.beans.factory.access.BeanFactoryReference;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

/**
 * {@link BeanFactoryLocator} used for storing Spring application context/bean factory for Gemfire
 * user components (or {@link com.gemstone.gemfire.cache.Declarable}. As opposed to the "traditional"
 * {@link org.springframework.beans.factory.access.SingletonBeanFactoryLocator} this implementation does
 * not require any configuration file; it rather assume declaration inside an application context
 * (usually through {@link com.gemstone.gemfire.cache.CacheFactory} which it will store under the name
 * and aliases of the bean (so the same "registry" can be used for storing multiple BeanFactories). 
 * If there is only one BeanFactory registered then a null value can be used with {@link #setBeanName(String)}.
 * 
 * In most cases, one does not need to use this class directly as it is used internally
 * by {@link com.gemstone.gemfire.cache.CacheFactory}.
 * 
 * @author Costin Leau
 * @author John Blum
 */
public class GemfireBeanFactoryLocator implements BeanFactoryLocator, BeanFactoryAware, BeanNameAware, DisposableBean,
		InitializingBean {

	private static final Log log = LogFactory.getLog(GemfireBeanFactoryLocator.class);

	// alias/bean name <-> BeanFactory lookup
	private static final ConcurrentMap<String, BeanFactory> beanFactories = new ConcurrentHashMap<String, BeanFactory>();

	// default factory to return
	private static volatile boolean canUseDefaultBeanFactory = true;
	private static volatile BeanFactory defaultFactory = null;

	private static class SimpleBeanFactoryReference implements BeanFactoryReference {

		private BeanFactory beanFactory;

		SimpleBeanFactoryReference(BeanFactory beanFactory) {
			this.beanFactory = beanFactory;
		}

		public BeanFactory getFactory() {
			Assert.state(beanFactory != null, "The BeanFactory has already been released or closed");
			return beanFactory;
		}

		public void release() throws FatalBeanException {
			beanFactory = null;
		}
	}

	private BeanFactory beanFactory;
	private String[] names;

	// default factory name
	private String factoryName = GemfireBeanFactoryLocator.class.getName();

	public void afterPropertiesSet() {
		// add the factory as default if possible (if it's the only one)
		synchronized (GemfireBeanFactoryLocator.class) {
			canUseDefaultBeanFactory = beanFactories.isEmpty();
			if (canUseDefaultBeanFactory) {
				if (defaultFactory == null) {
					defaultFactory = beanFactory;
					if (log.isDebugEnabled())
						log.debug("default beanFactoryReference=" + defaultFactory);
				}
				else {
					if (log.isDebugEnabled())
						log.debug("more then one beanFactory - default not possible to determine");
					canUseDefaultBeanFactory = false;
					defaultFactory = null;
				}
			}
		}

		// add aliases
		if (StringUtils.hasText(factoryName)) {
			String[] aliases = beanFactory.getAliases(factoryName);
			names = ObjectUtils.addObjectToArray(aliases, factoryName);

			for (String name : names) {
				if (log.isDebugEnabled())
					log.debug("adding key=" + name + " w/ reference=" + beanFactory);

				if (beanFactories.containsKey(name) && !beanFactory.equals(beanFactories.get(name))
						|| beanFactories.putIfAbsent(name, beanFactory) != null) {
					throw new IllegalArgumentException("a beanFactoryReference already exists for key " + factoryName);
				}
			}
		}
	}

	public void destroy() {
		if (names != null) {
			for (String name : names) {
				beanFactories.remove(name);
			}
		}
		if (beanFactory == defaultFactory) {
			synchronized (GemfireBeanFactoryLocator.class) {
				defaultFactory = null;
				canUseDefaultBeanFactory = beanFactories.isEmpty();
			}
		}
	}

	public BeanFactoryReference useBeanFactory(final String factoryKey) throws BeansException {
		// see if there is a default FactoryBean
		BeanFactory factory;

		if (!StringUtils.hasText(factoryKey)) {
			if (!canUseDefaultBeanFactory)
				throw new IllegalArgumentException(
						"a non-null factoryKey needs to be specified as there are more then one factoryKeys available; "
								+ beanFactories.keySet());
			factory = defaultFactory;
		}
		else {
			factory = beanFactories.get(factoryKey);
			if (factory == null)
				throw new IllegalArgumentException("there is no beanFactory under key " + factoryKey);
		}

		return new SimpleBeanFactoryReference(factory);
	}

	public void setBeanName(String name) {
		factoryName = name;
	}

	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

}
