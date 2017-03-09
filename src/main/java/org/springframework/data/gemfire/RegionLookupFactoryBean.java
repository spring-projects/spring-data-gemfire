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

import java.util.Optional;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} for looking up generic GemFire {@link Region Regions}. If lookups are not enabled
 * or the {@link Region} does not exist, an Exception is thrown.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.Region
 */
@SuppressWarnings("unused")
public abstract class RegionLookupFactoryBean<K, V>
		implements FactoryBean<Region<K, V>>, InitializingBean, BeanNameAware {

	protected final Log log = LogFactory.getLog(getClass());

	private Boolean lookupEnabled = false;

	private GemFireCache cache;

	private Region<?, ?> parent;

	private volatile Region<K, V> region;

	private String beanName;
	private String name;
	private String regionName;

	/**
	 * @inheritDoc
	 */
	@Override
	@SuppressWarnings("all")
	public void afterPropertiesSet() throws Exception {
		Assert.notNull(this.cache, "A 'Cache' reference must be set");

		String regionName = resolveRegionName();

		Assert.hasText(regionName, "'regionName', 'name' or 'beanName' property must be set");

		synchronized (this.cache) {
			if (isLookupEnabled()) {
				this.region = Optional.ofNullable(getParent())
					.map(parentRegion -> parentRegion.<K, V>getSubregion(regionName))
					.orElseGet(() -> this.cache.<K, V>getRegion(regionName));
			}

			if (region != null) {
				log.info(String.format("Found Region [%1$s] in Cache [%2$s]", regionName, cache.getName()));
			}
			else {
				log.info(String.format("Falling back to creating Region [%1$s] in Cache [%2$s]",
					regionName, cache.getName()));

				region = lookupRegion(cache, regionName);
			}
		}
	}

	/**
	 * Method to perform a lookup when the named {@link Region} does not exist.  By default, this implementation
	 * throws an exception.
	 *
	 * @param cache reference to the GemFire cache.
	 * @param regionName name of the GemFire {@link Region}.
	 * @return the {@link Region} in the GemFire cache with the given name.
	 * @throws BeanInitializationException if the lookup operation fails.
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<K, V> lookupRegion(GemFireCache cache, String regionName) throws Exception {
		throw new BeanInitializationException(String.format(
			"Region [%1$s] in Cache [%2$s] not found", regionName, cache));
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Region<K, V> getObject() throws Exception {
		return getRegion();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Class<?> getObjectType() {
		Region region = getRegion();
		return (region != null ? region.getClass() : Region.class);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Resolves the name of the GemFire {@link Region}.
	 *
	 * @return a {@link String} indicating the name of the GemFire {@link Region}.
	 * @see org.apache.geode.cache.Region#getName()
	 */
	public String resolveRegionName() {
		return (StringUtils.hasText(this.regionName) ? this.regionName
			: (StringUtils.hasText(this.name) ? this.name : this.beanName));
	}

	/**
	 * Sets the name of the {@link Region} based on the bean 'id' attribute.  If no {@link Region} is found
	 * with the given name, a new one will be created.
	 *
	 * @param name name of this {@link Region} bean in the Spring {@link org.springframework.context.ApplicationContext}.
	 * @see org.springframework.beans.factory.BeanNameAware#setBeanName(String)
	 */
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Sets a reference to the {@link GemFireCache} used to create the {@link Region}.
	 *
	 * @param cache reference to the {@link GemFireCache}.
	 * @see org.springframework.data.gemfire.CacheFactoryBean
	 * @see org.apache.geode.cache.GemFireCache
	 */
	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	/* (non-Javadoc) */
	boolean isLookupEnabled() {
		return Boolean.TRUE.equals(getLookupEnabled());
	}

	/* (non-Javadoc) */
	public void setLookupEnabled(Boolean lookupEnabled) {
		this.lookupEnabled = lookupEnabled;
	}

	/* (non-Javadoc) */
	public Boolean getLookupEnabled() {
		return lookupEnabled;
	}

	/**
	 * Sets the name of the cache {@link Region} based on the bean 'name' attribute.  If no {@link Region} is found
	 * with the given name, a new one will be created.  If no name is given, the value of the 'beanName' property
	 * will be used.
	 *
	 * @param name {@link Region} name.
	 * @see #setBeanName(String)
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * Sets a reference to the parent {@link Region} to indicated this {@link FactoryBean} represents a GemFire cache
	 * {@link Region Sub-Region}.
	 *
	 * @param parent reference to the parent {@link Region}.
	 * @see org.apache.geode.cache.Region
	 */
	public void setParent(Region<?, ?> parent) {
		this.parent = parent;
	}

	/**
	 * Returns a reference to the parent {@link Region} indicating this {@link FactoryBean} represents a GemFire cache
	 * {@link Region Sub-Region}.
	 *
	 * @return a reference to the parent {@link Region} or {@literal null} if this {@link Region}
	 * is not a {@link Region Sub-Region}.
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<?, ?> getParent() {
		return this.parent;
	}

	/**
	 * Returns a reference to the GemFire {@link Region} resolved by this Spring {@link FactoryBean}
	 * during the lookup operation; maybe a new {@link Region}.
	 *
	 * @return a reference to the GemFire {@link Region} resolved during lookup.
	 * @see org.apache.geode.cache.Region
	 */
	protected Region<K, V> getRegion() {
		return this.region;
	}

	/**
	 * Sets the name of the cache {@link Region}.  If no {@link Region} is found with the given name,
	 * a new one will be created.  If no name is given, the value of the 'name' property will be used.
	 *
	 * @param regionName name of the {@link Region}.
	 * @see #setName(String)
	 */
	public void setRegionName(String regionName) {
		this.regionName = regionName;
	}
}
