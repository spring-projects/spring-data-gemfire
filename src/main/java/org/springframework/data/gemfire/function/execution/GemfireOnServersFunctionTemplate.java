/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;


import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Optional;

import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolManager;
import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.Function;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.util.CacheUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Creates an {@literal OnServers} {@link Function} {@link Execution} initialized with
 * either a {@link RegionService cache} or a {@link Pool}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.RegionService
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.execute.Execution
 * @see org.apache.geode.cache.execute.Function
 * @see org.springframework.data.gemfire.function.execution.AbstractFunctionTemplate
 */
@SuppressWarnings("unused")
public class GemfireOnServersFunctionTemplate extends AbstractFunctionTemplate {

	private Pool pool;

	private final RegionService cache;

	private String poolName;

	public GemfireOnServersFunctionTemplate(RegionService cache) {

		Assert.notNull(cache, "RegionService must not be null");

		this.cache = cache;
	}

	public GemfireOnServersFunctionTemplate(Pool pool) {
		this.cache = resolveClientCache();
		this.pool = pool;
	}

	public GemfireOnServersFunctionTemplate(String poolName) {
		this.cache = resolveClientCache();
		this.poolName = poolName;
	}

	public void setPool(Pool pool) {
		this.pool = pool;
	}

	public void setPoolName(String poolName) {
		this.poolName = poolName;
	}

	@Override
	protected AbstractFunctionExecution getFunctionExecution() {

		Object gemfireObject = resolveRequiredGemFireObject();

		return gemfireObject instanceof Pool
			? new PoolServersFunctionExecution((Pool) gemfireObject)
			: new ServersFunctionExecution((RegionService) gemfireObject);
	}

	protected Object resolveRequiredGemFireObject() {
		return Optional.<Object>ofNullable(resolvePool()).orElseGet(this::resolveClientCache);
	}

	protected ClientCache resolveClientCache() {

		return Optional.ofNullable(CacheUtils.getClientCache())
			.orElseThrow(() -> newIllegalStateException("No ClientCache instance is present"));
	}

	protected Pool resolveDefaultPool() {

		return Optional.ofNullable(PoolManager.find(GemfireUtils.DEFAULT_POOL_NAME))
			.orElseThrow(() -> newIllegalStateException("No Pool was configured"));
	}

	protected Pool resolveNamedPool() {

		if (StringUtils.hasText(this.poolName)) {
			this.pool = Optional.ofNullable(PoolManager.find(this.poolName))
				.orElseThrow(() -> newIllegalStateException("No Pool with name [%s] exists",
					this.poolName));
		}

		return this.pool;
	}

	protected Pool resolvePool() {

		this.pool = Optional.ofNullable(this.pool)
			.orElseGet(this::resolveNamedPool);

		return this.pool;
	}
}
