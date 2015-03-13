/*
 * Copyright 2010-2013 the original author or authors.
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
 * Simple FactoryBean for retrieving generic GemFire {@link Region}s. If the Region does not exist,
 * an exception is thrown.  For declaring and configuring new regions, see {@link RegionFactoryBean}.
 *
 * @author Costin Leau
 * @author John Blum
 */
@SuppressWarnings("unused")
public abstract class RegionLookupFactoryBean<K, V> implements FactoryBean<Region<K, V>>, InitializingBean, BeanNameAware {

	protected final Log log = LogFactory.getLog(getClass());

	private Boolean lookupEnabled = Boolean.TRUE;

	private GemFireCache cache;

	private Region<?, ?> parent;
	private Region<K, V> region;

	private String beanName;
	private String name;
	private String regionName;

	public void afterPropertiesSet() throws Exception {
		Assert.notNull(cache, "The 'cache' property must be set.");

		String regionName = (StringUtils.hasText(this.regionName) ? this.regionName
			: (StringUtils.hasText(name) ? name : beanName));

		Assert.hasText(regionName, "The 'regionName', 'name' or 'beanName' property must be set.");

		synchronized (cache) {
			//region = (getParent() != null ? getParent().getSubregion(regionName) : cache.getRegion(regionName));
			if (isLookupEnabled()) {
				if (getParent() != null) {
					region = getParent().getSubregion(regionName);
				}
				else {
					region = cache.getRegion(regionName);
				}
			}

			if (region != null) {
				log.info(String.format("Retrieved Region [%1$s] from Cache [%2$s].", regionName, cache.getName()));
			}
			else {
				region = lookupFallback(cache, regionName);
			}
		}
	}

	/**
	 * Fallback method in case the named Region does not exist.  By default, this implementation throws an exception.
	 * 
	 * @param cache a reference to the GemFire Cache.
	 * @param regionName the name of the GemFire Cache Region.
	 * @return the Region in the GemFire Cache with the given name.
	 * @throws Exception if the lookup operation fails.
	 */
	protected Region<K, V> lookupFallback(GemFireCache cache, String regionName) throws Exception {
		throw new BeanInitializationException(String.format("Cannot find Region [%1$s] in Cache [%2$s].",
			regionName, cache));
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

	/**
	 * Sets the name of the Cache Region based on the bean 'id' attribute.  If no Region is found for the given name,
	 * a new one will be created.
	 *
	 * @param name the name of this bean (Region) in the application context (bean factory).
	 * @see org.springframework.beans.factory.BeanNameAware#setBeanName(String)
	 */
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Sets a reference to the Cache used to create the Region.
	 *
	 * @param cache a reference to the Cache.
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 * @see com.gemstone.gemfire.cache.GemFireCache
	 */
	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	/**
	 * Sets the name of the Cache Region based on the bean 'name' attribute.  If no Region is found with the given name,
	 * a new one will be created.  If no name is given, the value of the 'beanName' property will be used.
	 *
	 * @param name the region name
	 * @see #setBeanName(String)
	 * @see com.gemstone.gemfire.cache.Region#getFullPath()
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Sets a reference to the parent Region if this FactoryBean represents a GemFire Cache Sub-Region.
	 *
	 * @param parent a reference to the parent Region if this Region is a Sub-Region.
	 * @see com.gemstone.gemfire.cache.Region
	 */
	public void setParent(Region<?, ?> parent) {
		this.parent = parent;
	}

	/**
	 * Gets a reference to the parent Region if this FactoryBean represents a GemFire Cache Sub-Region.
	 *
	 * @return a reference to the parent Region or null if this Region is not a Sub-Region.
	 * @see com.gemstone.gemfire.cache.Region
	 */
	protected Region<?, ?> getParent() {
		return parent;
	}

	/**
	 * Sets the name of the Cache Region as expected by GemFire.  If no Region is found with the given name, a new one
	 * will be created.  If no name is given, the value of the 'name' property will be used.
	 *
	 * @param regionName a String indicating the name of the Region in GemFire.
	 * @see #setName(String)
	 * @see com.gemstone.gemfire.cache.Region#getName()
	 */
	public void setRegionName(String regionName) {
		this.regionName = regionName;
	}

	private boolean isLookupEnabled() {
		return Boolean.TRUE.equals(getLookupEnabled());
	}

	public Boolean getLookupEnabled() {
		return lookupEnabled;
	}

	public void setLookupEnabled(Boolean lookupEnabled) {
		this.lookupEnabled = lookupEnabled;
	}

	protected Region<K, V> getRegion() {
		return region;
	}

}
