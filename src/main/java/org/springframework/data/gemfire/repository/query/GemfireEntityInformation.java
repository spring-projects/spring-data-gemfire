/*
 * Copyright 2012 the original author or authors.
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

package org.springframework.data.gemfire.repository.query;

import org.apache.geode.cache.Region;
import org.springframework.data.repository.core.EntityInformation;

/**
 * {@link EntityInformation} capturing GemFire specific information.
 *
 * @author Oliver Gierke
 * @see org.springframework.data.repository.core.EntityInformation
 */
public interface GemfireEntityInformation<T, ID> extends EntityInformation<T, ID> {

	/**
	 * Returns the name of the {@link Region} the entity is held in.
	 *
	 * @return the name of the {@link Region} the entity is held in.
	 */
	String getRegionName();

}
