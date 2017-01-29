/*
 * Copyright 2002-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;

import java.util.Set;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.FunctionService;
import org.springframework.util.CollectionUtils;

/**
 * Creates a GemFire {@link Execution} using {code}FunctionService.onRegion(Region region){code}
 * @author David Turanski
 *
 */
class RegionFunctionExecution extends AbstractFunctionExecution {


	private final Region<?, ?> region;
	private volatile Set<?> keys;

	public RegionFunctionExecution(Region<?, ?> region) {
		super();
		this.region = region;
	}

	public RegionFunctionExecution setKeys(Set<?> keys) {
		this.keys = keys;
		return this;
	}

	protected Set<?> getKeys() {
		return this.keys;
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.FunctionExecution#getExecution()
	 */
	@Override
	protected Execution getExecution() {
		Execution execution = FunctionService.onRegion(region);
		if (!CollectionUtils.isEmpty(this.keys) ) {
			execution = execution.withFilter(keys);
		}
		return execution;
	}
}
