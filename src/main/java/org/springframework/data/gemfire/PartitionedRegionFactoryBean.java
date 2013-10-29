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

import java.util.concurrent.ConcurrentMap;

import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * @author David Turanski
 * @author John Blum
 */
public class PartitionedRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		if (dataPolicy != null) {
			DataPolicy resolvedDataPolicy = new DataPolicyConverter().convert(dataPolicy);

			Assert.notNull(resolvedDataPolicy, String.format("Data Policy '%1$s' is invalid.", dataPolicy));

			// Validate that the user-defined Data Policy matches the appropriate Spring GemFire XML namespace
			// configuration meta-data element for Region (i.e. <gfe:partitioned-region .../>)!
			Assert.isTrue(resolvedDataPolicy.withPartitioning(), String.format(
				"Data Policy '%1$s' is not supported in Partitioned Regions.", resolvedDataPolicy));

			// Validate that the data-policy and persistent attributes are compatible when specified!
			assertDataPolicyAndPersistentAttributesAreCompatible(resolvedDataPolicy);

			regionFactory.setDataPolicy(resolvedDataPolicy);
		}
		else if (isPersistent()) {
			// first, check the presence of GemFire 6.5 or Higher
			Assert.isTrue(isGemFireVersion65orHigher(), String.format(
				"Can define Persistent Partitioned Regions only from GemFire 6.5 onwards; current version is [%1$s]",
					CacheFactory.getVersion()));
			regionFactory.setDataPolicy(DataPolicy.PERSISTENT_PARTITION);
		}
		else {
			regionFactory.setDataPolicy(DataPolicy.PARTITION);
		}
	}

	private boolean isGemFireVersion65orHigher() {
		return ConcurrentMap.class.isAssignableFrom(Region.class);
	}

}
