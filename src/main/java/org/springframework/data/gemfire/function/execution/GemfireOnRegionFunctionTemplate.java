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
import org.apache.geode.cache.execute.Function;
import org.springframework.util.Assert;

/**
 * @author David Turanski
 * @author John Blum
 */
public class GemfireOnRegionFunctionTemplate extends AbstractFunctionTemplate implements GemfireOnRegionOperations {

	private final Region<?, ?> region;

	/**
	 * Constructs an instance of the GemFireOnRegionFunctionTemplate with the given GemFire Cache Region.
	 *
	 * @param region the GemFire Cache Region upon which the Function will be executed.
	 * @see org.apache.geode.cache.Region
	 */
	public GemfireOnRegionFunctionTemplate(Region<?, ?> region) {

		Assert.notNull(region, "Region must not be null");

		this.region = region;
	}

	@Override
	protected RegionFunctionExecution getFunctionExecution() {
		return new RegionFunctionExecution(this.region);
	}

	@Override
	public <T> Iterable<T> execute(Function function, Set<?> keys, Object... args) {

		return execute(getFunctionExecution()
			.setKeys(keys)
			.setFunction(function)
			.setTimeout(this.timeout)
			.setArgs(args));
	}

	@Override
	public <T> Iterable<T> execute(String functionId, Set<?> keys, Object... args) {

		return execute(getFunctionExecution()
			.setKeys(keys).setFunctionId(functionId)
			.setTimeout(this.timeout)
			.setArgs(args));
	}

	@Override
	public <T> T executeAndextract(String functionId, Set<?> keys, Object... args) {

		return executeAndExtract(getFunctionExecution()
			.setKeys(keys)
			.setFunctionId(functionId)
			.setTimeout(this.timeout).setArgs(args));
	}

	@Override
	public void executeWithNoResult(String functionId, Set<?> keys, Object... args) {

		execute(getFunctionExecution()
			.setKeys(keys)
			.setFunctionId(functionId)
			.setTimeout(this.timeout)
			.setArgs(args), false);
	}
}
