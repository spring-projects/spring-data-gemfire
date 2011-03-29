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

package org.springframework.data.gemfire;

import java.util.Properties;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.core.io.Resource;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.support.PersistenceExceptionTranslator;

import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.distributed.DistributedMember;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * Factory used for configuring a Gemfire Cache manager. Allows either retrieval of an existing, opened cache 
 * or the creation of a new one.

 * <p>This class implements the {@link org.springframework.dao.support.PersistenceExceptionTranslator}
 * interface, as auto-detected by Spring's
 * {@link org.springframework.dao.annotation.PersistenceExceptionTranslationPostProcessor},
 * for AOP-based translation of native exceptions to Spring DataAccessExceptions.
 * Hence, the presence of this class automatically enables
 * a PersistenceExceptionTranslationPostProcessor to translate GemFire exceptions.
 * 
 * @author Costin Leau
 */
public class CacheFactoryBean implements BeanNameAware, BeanFactoryAware, BeanClassLoaderAware, DisposableBean,
		InitializingBean, FactoryBean<Cache>, PersistenceExceptionTranslator {

	protected final Log log = LogFactory.getLog(getClass());

	private Cache cache;
	private Resource cacheXml;
	private Properties properties;
	private DistributedSystem system;
	private ClassLoader beanClassLoader;
	private GemfireBeanFactoryLocator factoryLocator = new GemfireBeanFactoryLocator();

	private BeanFactory beanFactory;
	private String beanName;

	public void afterPropertiesSet() throws Exception {
		// initialize locator
		factoryLocator.setBeanFactory(beanFactory);
		factoryLocator.setBeanName(beanName);
		factoryLocator.afterPropertiesSet();

		Properties cfgProps = mergeProperties();
		system = DistributedSystem.connect(cfgProps);

		DistributedMember member = system.getDistributedMember();
		log.info("Connected to Distributed System [" + system.getName() + "=" + member.getId() + "@" + member.getHost()
				+ "]");

		// use the bean class loader to load Declarable classes
		Thread th = Thread.currentThread();
		ClassLoader oldTCCL = th.getContextClassLoader();

		try {
			th.setContextClassLoader(beanClassLoader);
			// first look for open caches
			String msg = null;
			cache = fetchExistingCache(system);
			if (cache == null) {
				cache = createCache(system);
				msg = "Created";
			}
			else {
				msg = "Retrieved existing";
			}

			log.info(msg + " GemFire v." + CacheFactory.getVersion() + " Cache [" + cache.getName() + "]");

			// load/init cache.xml
			if (cacheXml != null) {
				cache.loadCacheXml(cacheXml.getInputStream());
			}

			if (log.isDebugEnabled())
				log.debug("Initialized cache from " + cacheXml);
		} finally {
			th.setContextClassLoader(oldTCCL);
		}
	}

	protected Cache fetchExistingCache(DistributedSystem system) {
		try {
			return CacheFactory.getInstance(system);
		} catch (CacheClosedException ex) {
			return null;
		}
	}

	protected Cache createCache(DistributedSystem system) throws Exception {
		return CacheFactory.create(system);
	}

	private Properties mergeProperties() {
		Properties cfgProps = (properties != null ? (Properties) properties.clone() : new Properties());
		return cfgProps;
	}

	public void destroy() throws Exception {
		if (cache != null && !cache.isClosed()) {
			cache.close();
		}
		cache = null;

		if (system != null && system.isConnected()) {
			DistributedSystem.releaseThreadsSockets();
			system.disconnect();
		}
		system = null;

		factoryLocator.destroy();
	}

	public DataAccessException translateExceptionIfPossible(RuntimeException ex) {
		if (ex instanceof GemFireException) {
			return GemfireCacheUtils.convertGemfireAccessException((GemFireException) ex);
		}
		if (ex instanceof IllegalArgumentException) {
			DataAccessException wrapped = GemfireCacheUtils.convertQueryExceptions(ex);
			// ignore conversion if the generic exception is returned
			if (!(wrapped instanceof GemfireSystemException)) {
				return wrapped;
			}
		}

		return null;
	}

	public Cache getObject() throws Exception {
		return cache;
	}

	public Class<? extends Cache> getObjectType() {
		return (cache != null ? cache.getClass() : Cache.class);
	}

	public boolean isSingleton() {
		return true;
	}

	protected ClassLoader getBeanClassLoader() {
		return beanClassLoader;
	}

	public void setBeanClassLoader(ClassLoader classLoader) {
		this.beanClassLoader = classLoader;
	}

	protected BeanFactory getBeanFactory() {
		return beanFactory;
	}

	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Sets the cache properties.
	 * 
	 * @param properties the properties to set
	 */
	public void setProperties(Properties properties) {
		this.properties = properties;
	}

	/**
	 * Sets the cache configuration.
	 * 
	 * @param cacheXml the cacheXml to set
	 */
	public void setCacheXml(Resource cacheXml) {
		this.cacheXml = cacheXml;
	}
}