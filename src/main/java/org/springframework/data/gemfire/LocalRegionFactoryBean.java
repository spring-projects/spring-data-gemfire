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
import org.apache.geode.cache.Scope;
import org.springframework.util.Assert;

/**
 * @author David Turanski
 * @author John Blum
 */
public class LocalRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	@Override
	public void setScope(Scope scope) {
		throw new UnsupportedOperationException("Setting the Scope on Local Regions is not allowed.");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.setScope(Scope.LOCAL);
		super.afterPropertiesSet();
	}

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, DataPolicy dataPolicy) {
		if (dataPolicy == null || DataPolicy.NORMAL.equals(dataPolicy)) {
			// NOTE this is safe since a LOCAL Scoped NORMAL Region requiring persistence can be satisfied with
			// PERSISTENT_REPLICATE, per the RegionShortcut.LOCAL_PERSISTENT
			DataPolicy resolvedDataPolicy = (isPersistent() ? DataPolicy.PERSISTENT_REPLICATE : DataPolicy.NORMAL);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
		else if (DataPolicy.PRELOADED.equals(dataPolicy)) {
			// NOTE this is safe since a LOCAL Scoped PRELOADED Region requiring persistence can be satisfied with
			// PERSISTENT_REPLICATE, per the RegionShortcut.LOCAL_PERSISTENT
			DataPolicy resolvedDataPolicy = (isPersistent() ? DataPolicy.PERSISTENT_REPLICATE : DataPolicy.PRELOADED);

			regionFactory.setDataPolicy(resolvedDataPolicy);
			setDataPolicy(resolvedDataPolicy);
		}
		else if (DataPolicy.PERSISTENT_REPLICATE.equals(dataPolicy)
				&& RegionShortcutWrapper.valueOf(getShortcut()).isPersistent()) {
			regionFactory.setDataPolicy(dataPolicy);
			setDataPolicy(dataPolicy);
		}
		else {
			throw new IllegalArgumentException(String.format("Data Policy '%1$s' is not supported for Local Regions.",
				dataPolicy));
		}
	}

	/**
	 * Resolves the Data Policy used by this "local" GemFire Region (i.e. locally Scoped; Scope.LOCAL) based on the
	 * enumerated value from org.apache.geode.cache.RegionShortcuts (LOCAL, LOCAL_PERSISTENT, LOCAL_HEAP_LRU,
	 * LOCAL_OVERFLOW, and LOCAL_PERSISTENT_OVERFLOW), but without consideration of the Eviction settings.
	 *
	 * @param regionFactory the GemFire RegionFactory used to created the Local Region.
	 * @param persistent a boolean value indicating whether the Local Region should persist it's data.
	 * @param dataPolicy requested Data Policy as set by the user in the Spring GemFire configuration meta-data.
	 * @see org.apache.geode.cache.DataPolicy
	 * @see org.apache.geode.cache.RegionFactory
	 * @see org.apache.geode.cache.RegionShortcut
	 */
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
