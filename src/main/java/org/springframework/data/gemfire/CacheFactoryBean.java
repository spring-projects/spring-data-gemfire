/*
 * Copyright 2010-2011 the original author or authors.
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
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.distributed.DistributedMember;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.pdx.PdxSerializable;
import com.gemstone.gemfire.pdx.PdxSerializer;

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

	private class PdxOptions implements Runnable {

		private CacheFactory factory;

		PdxOptions(CacheFactory factory) {
			this.factory = factory;
		}

		public void run() {
			Assert.isAssignable(PdxSerializer.class, pdxSerializer.getClass(), "Invalid pdx serializer used");
			factory.setPdxSerializer((PdxSerializer) pdxSerializer);
			factory.setPdxDiskStore(pdxDiskStoreName);
			factory.setPdxIgnoreUnreadFields(pdxIgnoreUnreadFields);
			factory.setPdxPersistent(pdxPersistent);
			factory.setPdxReadSerialized(pdxReadSerialized);
		}
	}

	private static final Log log = LogFactory.getLog(CacheFactoryBean.class);

	private Cache cache;
	private Resource cacheXml;
	private Properties properties;
	private ClassLoader beanClassLoader;
	private GemfireBeanFactoryLocator factoryLocator;

	private BeanFactory beanFactory;
	private String beanName;
	private boolean useBeanFactoryLocator = true;
	// PDX options
	private Object pdxSerializer;
	private Boolean pdxPersistent;
	private Boolean pdxReadSerialized;
	private Boolean pdxIgnoreUnreadFields;
	private String pdxDiskStoreName;

	public void afterPropertiesSet() throws Exception {
		// initialize locator
		if (useBeanFactoryLocator) {
			factoryLocator = new GemfireBeanFactoryLocator();
			factoryLocator.setBeanFactory(beanFactory);
			factoryLocator.setBeanName(beanName);
			factoryLocator.afterPropertiesSet();
		}
		Properties cfgProps = mergeProperties();
		CacheFactory factory = new CacheFactory(cfgProps);

		// use the bean class loader to load Declarable classes
		Thread th = Thread.currentThread();
		ClassLoader oldTCCL = th.getContextClassLoader();

		try {
			th.setContextClassLoader(beanClassLoader);
			// first look for open caches
			String msg = null;
			try {
				cache = CacheFactory.getAnyInstance();
				msg = "Retrieved existing";
			} catch (CacheClosedException ex) {
				// GemFire 6.6 specific options
				if (pdxSerializer != null || pdxPersistent != null || pdxReadSerialized != null
						|| pdxIgnoreUnreadFields != null || pdxDiskStoreName != null) {
					Assert.isTrue(ClassUtils.isPresent("com.gemstone.gemfire.pdx.PdxSerializer", beanClassLoader),
							"Cannot set PDX options since GemFire 6.6 not detected");
					new PdxOptions(factory).run();
				}

				// fall back to cache creation
				cache = factory.create();
				msg = "Created";
			}

			DistributedSystem system = cache.getDistributedSystem();
			DistributedMember member = system.getDistributedMember();
			log.info("Connected to Distributed System [" + system.getName() + "=" + member.getId() + "@"
					+ member.getHost() + "]");


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

	private Properties mergeProperties() {
		Properties cfgProps = (properties != null ? (Properties) properties.clone() : new Properties());
		return cfgProps;
	}

	public void destroy() throws Exception {
		if (cache != null && !cache.isClosed()) {
			cache.close();
		}

		cache = null;

		if (factoryLocator != null) {
			factoryLocator.destroy();
			factoryLocator = null;
		}
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

	public void setBeanClassLoader(ClassLoader classLoader) {
		this.beanClassLoader = classLoader;
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

	/**
	 * Indicates whether a bean factory locator is enabled (default) for this cache definition or not. The locator stores
	 * the enclosing bean factory reference to allow auto-wiring of Spring beans into GemFire managed classes. Usually disabled
	 * when the same cache is used in multiple application context/bean factories inside the same VM.
	 * 
	 * @param usage true if the bean factory locator is used underneath or not
	 */
	public void setUseBeanFactoryLocator(boolean usage) {
		this.useBeanFactoryLocator = usage;
	}

	/**
	 * Sets the {@link PdxSerializable} for this cache. Applicable on GemFire 6.6 or higher.
	 * The argument is of type object for compatibility with GemFire 6.5.
	 * 
	 * @param serializer pdx serializer configured for this cache.
	 */
	public void setPdxSerializer(Object serializer) {
		this.pdxSerializer = serializer;
	}

	/**
	 * Sets the object preference to PdxInstance type. Applicable on GemFire 6.6 or higher.
	 *  
	 * @param pdxPersistent the pdxPersistent to set
	 */
	public void setPdxPersistent(Boolean pdxPersistent) {
		this.pdxPersistent = pdxPersistent;
	}

	/**
	 * Controls whether the type metadata for PDX objects is persisted to disk. Applicable on GemFire 6.6 or higher.
	 * 
	 * @param pdxReadSerialized the pdxReadSerialized to set
	 */
	public void setPdxReadSerialized(Boolean pdxReadSerialized) {
		this.pdxReadSerialized = pdxReadSerialized;
	}

	/**
	 * Controls whether pdx ignores fields that were unread during deserialization. Applicable on GemFire 6.6 or higher.
	 * 
	 * @param pdxIgnoreUnreadFields the pdxIgnoreUnreadFields to set
	 */
	public void setPdxIgnoreUnreadFields(Boolean pdxIgnoreUnreadFields) {
		this.pdxIgnoreUnreadFields = pdxIgnoreUnreadFields;
	}

	/**
	 * Set the disk store that is used for PDX meta data. Applicable on GemFire 6.6 or higher.
	 *  
	 * @param pdxDiskStoreName the pdxDiskStoreName to set
	 */
	public void setPdxDiskStoreName(String pdxDiskStoreName) {
		this.pdxDiskStoreName = pdxDiskStoreName;
	}
}