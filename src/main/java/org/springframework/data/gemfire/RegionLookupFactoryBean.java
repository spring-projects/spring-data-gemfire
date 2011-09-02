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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

/**
 * Simple FactoryBean for retrieving generic GemFire {@link Region}s. If the region doesn't exist, an exception is thrown.
 * 
 * For declaring and configuring new regions, see {@link RegionFactoryBean}.
 * 
 * @author Costin Leau
 */
public class RegionLookupFactoryBean<K, V> implements FactoryBean<Region<K, V>>, InitializingBean, BeanNameAware {

	protected final Log log = LogFactory.getLog(getClass());

	private String beanName;
	private GemFireCache cache;
	private String name;

	private Region<K, V> region;

	public void afterPropertiesSet() throws Exception {
		Assert.notNull(cache, "Cache property must be set");
		name = (!StringUtils.hasText(name) ? beanName : name);
		Assert.hasText(name, "Name (or beanName) property must be set");

		region = cache.getRegion(name);
		if (region != null) {
			log.info("Retrieved region [" + name + "] from cache");
		}

		else {
			region = lookupFallback(cache, name);
		}
	}

	/**
	 * Fall back method in case the named region does not exist.
	 * By default, this implementation throws an exception.
	 * 
	 * @param cache GemFire cache
	 * @param regionName region name
	 * @throws Exception
	 */
	protected Region<K, V> lookupFallback(GemFireCache cache, String regionName) throws Exception {
		throw new BeanInitializationException("Cannot find region [" + regionName + "] in cache " + cache);
	}

	public Region<K, V> getObject() throws Exception {
		return region;
	}

	public Class<?> getObjectType() {
		return (region != null ? region.getClass() : Region.class);
	}

	public boolean isSingleton() {
		return true;
	}

	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Sets the cache used for creating the region.
	 * 
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 * @param cache the cache to set
	 */
	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	/**
	 * Sets the name of the cache region. If no cache is found under
	 * the given name, a new one will be created.
	 * If no name is given, the beanName will be used. 
	 * 
	 * @see com.gemstone.gemfire.cache.Region#getFullPath()
	 * @see #setBeanName(String)
	 * 
	 * @param name the region name
	 */
	public void setName(String name) {
		this.name = name;
	}

	protected Region<K, V> getRegion() {
		return region;
	}
}