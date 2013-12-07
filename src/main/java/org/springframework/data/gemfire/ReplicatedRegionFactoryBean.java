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

import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * @author David Turanski
 * @author John Blum
 */
public class ReplicatedRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		if (dataPolicy != null) {
			DataPolicy resolvedDataPolicy = new DataPolicyConverter().convert(dataPolicy);

			Assert.notNull(resolvedDataPolicy, String.format("Data Policy '%1$s' is invalid.", dataPolicy));

			if (DataPolicy.EMPTY.equals(resolvedDataPolicy)) {
				resolvedDataPolicy = DataPolicy.EMPTY;
			}
			else {
				// Validate that the user-defined Data Policy matches the appropriate Spring GemFire XML namespace
				// configuration meta-data element for Region (i.e. <gfe:replicated-region .../>)!
				Assert.isTrue(resolvedDataPolicy.withReplication(), String.format(
					"Data Policy '%1$s' is not supported in Replicated Regions.", resolvedDataPolicy));
			}

			// Validate that the data-policy and persistent attributes are compatible when specified!
			assertDataPolicyAndPersistentAttributesAreCompatible(resolvedDataPolicy);

			regionFactory.setDataPolicy(resolvedDataPolicy);
		}
		else {
			regionFactory.setDataPolicy(isPersistent() ? DataPolicy.PERSISTENT_REPLICATE : DataPolicy.REPLICATE);
		}
	}

}
