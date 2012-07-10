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

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.RegionFactory;

/**
 * @author David Turanski
 * 
 */
public class LocalRegionFactoryBean<K, V> extends RegionFactoryBean<K, V> {

	@Override
	protected void resolveDataPolicy(RegionFactory<K, V> regionFactory, boolean persistent, String dataPolicyName) {
		if (dataPolicyName != null) {
			if ("NORMAL".equals(dataPolicyName) || dataPolicyName == null) {
				regionFactory.setDataPolicy(DataPolicy.NORMAL);
			}
			else if ("PRELOADED".equals(dataPolicyName)) {
				regionFactory.setDataPolicy(DataPolicy.PRELOADED);
			}
			else if ("EMPTY".equals(dataPolicyName)) {
				regionFactory.setDataPolicy(DataPolicy.EMPTY);
			}
			else {
				throw new IllegalArgumentException("Data policy '" + dataPolicyName
						+ "' is unsupported or invalid for local regions.");
			}
		}
	}

}
