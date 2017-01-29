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

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.FunctionService;
import org.springframework.util.Assert;

/**
 * Creates a GemFire {@link Execution} using {code}FunctionService.onServers(Pool pool){code}
 * @author David Turanski
 *
 */
class PoolServersFunctionExecution extends AbstractFunctionExecution {


	private final Pool pool;

    /**
     * @param pool the {@link Pool}
     */
	public PoolServersFunctionExecution(Pool pool ) {
		super();
		Assert.notNull(pool, "pool cannot be null");
		this.pool = pool;
	}

	@Override
	protected Execution getExecution() {
		return FunctionService.onServers(this.pool);
	}
}
