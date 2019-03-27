/* Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config;

/**
 * The GemfireConstants class define constants for Spring GemFire component bean names.
 *
 * @author David Turanski
 * @author John Blum
 */
@SuppressWarnings("unused")
public interface GemfireConstants {

	String DEFAULT_GEMFIRE_CACHE_NAME = "gemfireCache";
	String DEFAULT_GEMFIRE_FUNCTION_SERVICE_NAME = "gemfireFunctionService";
	String DEFAULT_GEMFIRE_POOL_NAME = "DEFAULT";
	String DEFAULT_GEMFIRE_TRANSACTION_MANAGER_NAME = "gemfireTransactionManager";

	@Deprecated
	String DEFAULT_GEMFIRE_TXMANAGER_NAME = DEFAULT_GEMFIRE_TRANSACTION_MANAGER_NAME;

}
