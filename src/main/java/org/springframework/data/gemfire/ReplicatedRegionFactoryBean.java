/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.RegionFactory;
import org.springframework.util.Assert;

/**
 * @author David Turanski
 * @author John Blum
 */
public class ReplicatedRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, DataPolicy dataPolicy) {
		if (dataPolicy == null) {
			dataPolicy = (isPersistent() ? DataPolicy.PERSISTENT_REPLICATE : DataPolicy.REPLICATE);
		}
		else if (DataPolicy.EMPTY.equals(dataPolicy)) {
			dataPolicy = DataPolicy.EMPTY;
		}
		else {
			// Validate that the user-defined Data Policy matches the appropriate Spring GemFire XML namespace
			// configuration meta-data element for the Region (i.e. <gfe:replicated-region .../>)!
			Assert.isTrue(dataPolicy.withReplication(), String.format(
				"Data Policy '%1$s' is not supported in Replicated Regions.", dataPolicy));
		}

		// Validate that the data-policy and persistent attributes are compatible when both are specified!
		assertDataPolicyAndPersistentAttributesAreCompatible(dataPolicy);

		regionFactory.setDataPolicy(dataPolicy);
		setDataPolicy(dataPolicy);
	}

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		DataPolicy resolvedDataPolicy = null;

		if (dataPolicy != null) {
			resolvedDataPolicy = new DataPolicyConverter().convert(dataPolicy);
			Assert.notNull(resolvedDataPolicy, String.format("Data Policy '%1$s' is invalid.", dataPolicy));
		}

		resolveDataPolicy(regionFactory, persistent, resolvedDataPolicy);
	}

}
