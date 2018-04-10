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
import org.apache.geode.cache.client.PoolManager;
import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.FunctionService;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

/**
 * Creates an {@link Execution} from {@link FunctionService#onServer(Pool)}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionExecution
 */
class PoolServerFunctionExecution extends AbstractFunctionExecution implements InitializingBean {

	private Pool pool;

	private String poolName;

    /**
	 * Constructs a new instance of {@link PoolServerFunctionExecution} initialized with the given {@link Pool}.
	 *
     * @param pool {@link Pool} used to initialize the {@link Execution}.
	 * @throws IllegalArgumentException if {@link Pool} is {@literal null}.
	 * @see org.apache.geode.cache.client.Pool
     */
	public PoolServerFunctionExecution(Pool pool) {

		Assert.notNull(pool, "Pool must not be null");

		this.pool = pool;
	}

	/**
	 * Constructs a new instance of {@link PoolServerFunctionExecution} initialized with
	 * the given {@link String name} of the {@link Pool}.
	 *
	 * @param poolName {@link String} containing the name of the {@link Pool}
	 * used to initialize the {@link Execution}.
	 * @throws IllegalArgumentException if {@link String poolName} is {@literal null} or empty.
	 */
	public PoolServerFunctionExecution(String poolName) {

		Assert.hasText(poolName, "Pool name must not be null or empty");

		this.poolName = poolName;
	}

	@Override
	protected Execution getExecution() {
		return FunctionService.onServer(this.pool);
	}

	@Override
	public void afterPropertiesSet() throws Exception {

		this.pool = PoolManager.find(this.poolName);

		Assert.notNull(this.pool,String.format("Pool [%s] not found", this.poolName));
	}
}
