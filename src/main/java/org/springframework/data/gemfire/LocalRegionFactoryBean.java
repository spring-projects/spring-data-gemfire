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
import com.gemstone.gemfire.cache.Scope;

/**
 * @author David Turanski
 * 
 */
public class LocalRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {
	@Override
	public void setScope(Scope scope) {
		throw new UnsupportedOperationException("setScope() is not allowed for Local Regions");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.setScope(Scope.LOCAL);
		super.afterPropertiesSet();
	}

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, Boolean persistent, String dataPolicy) {
		if (dataPolicy != null) {
			dataPolicy = dataPolicy.toUpperCase();
			if ("NORMAL".equals(dataPolicy) || dataPolicy == null) {
				regionFactory.setDataPolicy(DataPolicy.NORMAL);
			}
			else if ("PRELOADED".equals(dataPolicy)) {
				regionFactory.setDataPolicy(DataPolicy.PRELOADED);
			}
			else if ("EMPTY".equals(dataPolicy)) {
				Assert.isTrue(persistent == null || !persistent, "Cannot have persistence on an empty region");
				regionFactory.setDataPolicy(DataPolicy.EMPTY);
			}
			else {
				throw new IllegalArgumentException("Data policy '" + dataPolicy
						+ "' is unsupported or invalid for local regions.");
			}
		}
	}

}
