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

package org.springframework.data.gemfire.client;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.Pool;

/**
 * Client extension for GemFire regions.
 * 
 * @author Costin Leau
 */
public class ClientRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> implements BeanFactoryAware {

	private Interest<K>[] interests;
	private String poolName;
	private BeanFactory beanFactory;

	@Override
	protected void postProcess(Region<K, V> region) {
		if (!ObjectUtils.isEmpty(interests)) {
			for (Interest<K> interest : interests) {
				if (interest instanceof RegexInterest) {
					// do the cast since it's safe
					region.registerInterestRegex((String) interest.getKey(), interest.getPolicy(), interest.isDurable());
				}
				else {
					region.registerInterest(interest.getKey(), interest.getPolicy(), interest.isDurable());
				}
			}
		}
	}

	@Override
	protected void postProcess(AttributesFactory<K, V> attrFactory) {
		// try to eagerly initialize the pool name, if defined as a bean
		if (beanFactory.isTypeMatch(poolName, Pool.class)) {
			if (log.isDebugEnabled()) {
				log.debug("Found bean definition for pool '" + poolName + "'. Eagerly initializing it...");
			}
			beanFactory.getBean(poolName, Pool.class);
		}

		attrFactory.setPoolName(poolName);
	}

	@Override
	public void destroy() throws Exception {
		Region<K, V> region = getObject();
		// unregister interests
		try {
			if (region != null && !ObjectUtils.isEmpty(interests)) {
				for (Interest<K> interest : interests) {
					if (interest instanceof RegexInterest) {
						region.unregisterInterestRegex((String) interest.getKey());
					}
					else {
						region.unregisterInterest(interest.getKey());
					}
				}
			}
			// should not really happen since interests are validated at start/registration
		} catch (UnsupportedOperationException ex) {
			log.warn("Cannot unregister cache interests", ex);
		}

		super.destroy();
	}

	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	
	/**
	 * Set the interests for this client region. Both key and regex interest are supported.
	 * 
	 * @param interests the interests to set
	 */
	public void setInterests(Interest<K>[] interests) {
		this.interests = interests;
	}

	/**
	 * @return the interests
	 */
	Interest<K>[] getInterests() {
		return interests;
	}

	/**
	 * Sets the pool name used by this client.
	 * 
	 * @param poolName
	 */
	public void setPoolName(String poolName) {
		Assert.hasText(poolName, "pool name is required");
		this.poolName = poolName;
	}

	/**
	 * Sets the pool used by this client.
	 * 
	 * @param pool
	 */
	public void setPool(Pool pool) {
		Assert.notNull(pool, "pool cannot be null");
		setPoolName(pool.getName());
	}
}