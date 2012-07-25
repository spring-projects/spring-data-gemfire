/*
 * Copyright 2010-2012 the original author or authors.
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
 * 
 */
public class ReplicatedRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {
	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		DataPolicy dp = null;
		if (dataPolicy != null) {
			dataPolicy = dataPolicy.toUpperCase();
			if ("EMPTY".equals(dataPolicy)) {
				Assert.isTrue(persistent == null || !persistent, "Cannot have persistence on an empty region");
				dp = DataPolicy.EMPTY;
			}
			else {
				dp = new DataPolicyConverter().convert(dataPolicy);
				Assert.notNull(dp, "Data policy " + dataPolicy + " is invalid");
				Assert.isTrue(dp.withReplication(), "Data policy " + dataPolicy
						+ " is invalid or unsupported in replicated regions");
				if (isPersistent()) {
					Assert.isTrue(dp.withPersistence(), "Data policy " + dataPolicy
							+ " is invalid when persistent is false");
				}
			}
		}
		else {
			dp = (isPersistent()) ? DataPolicy.PERSISTENT_REPLICATE : DataPolicy.REPLICATE;
		}
		regionFactory.setDataPolicy(dp);
	}
}
