/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.util;

import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.distributed.internal.InternalDistributedSystem;

/**
 * DistributedSystemUtils is an abstract utility class for working with the GemFire DistributedSystem.
 *
 * @author John Blum
 * @see com.gemstone.gemfire.distributed.DistributedSystem
 * @since 1.7.0
 */
public abstract class DistributedSystemUtils {

	@SuppressWarnings("unchecked")
	public static <T extends DistributedSystem> T getDistributedSystem() {
		return (T) InternalDistributedSystem.getAnyInstance();
	}

	public static boolean isConnected(DistributedSystem distributedSystem) {
		return (distributedSystem != null && distributedSystem.isConnected());
	}

}
