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


import org.apache.geode.cache.RegionService;
import org.apache.geode.cache.client.Pool;

/**
 * @author David Turanski
 *
 */
public class GemfireOnServersFunctionTemplate  extends AbstractFunctionTemplate {

	private final RegionService cache;
	private final Pool pool;


	public GemfireOnServersFunctionTemplate (RegionService cache) {
		this.cache = cache;
		this.pool = null;
	}

	public GemfireOnServersFunctionTemplate (Pool pool) {
		this.pool = pool;
		this.cache = null;
	}

	@Override
	protected AbstractFunctionExecution getFunctionExecution() {
		 if (this.pool == null) {
			 return new ServersFunctionExecution(this.cache);
		 }
		 return new PoolServersFunctionExecution(this.pool);
	}

}
