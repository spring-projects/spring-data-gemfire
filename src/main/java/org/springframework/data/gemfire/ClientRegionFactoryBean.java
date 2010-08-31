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

import org.springframework.util.ObjectUtils;

import com.gemstone.gemfire.cache.Region;

/**
 * Client extension for Gemfire regions.
 * 
 * @author Costin Leau
 */
public class ClientRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	private Interest<K>[] interests;

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
}