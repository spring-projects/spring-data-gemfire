/*
 * Copyright 2002-2013 the original author or authors.
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
 * Constructs an {@link Execution} using {@link FunctionService#onServers(Pool)}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.execute.Execution
 * @see org.apache.geode.cache.execute.FunctionService
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionExecution
 */
class PoolServersFunctionExecution extends AbstractFunctionExecution {

	private final Pool pool;

	PoolServersFunctionExecution(Pool pool) {

		Assert.notNull(pool, "Pool must not be null");

		this.pool = pool;
	}

	protected Pool getPool() {
		return pool;
	}

	@Override
	protected Execution getExecution() {
		return FunctionService.onServers(getPool());
	}
}
