/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation.test.entities;

import org.springframework.data.gemfire.ScopeType;
import org.springframework.data.gemfire.mapping.annotation.ReplicateRegion;

/**
 * {@link ReplicateRegionEntity} persistent entity stored in the "Accounts"
 * {@link org.apache.geode.cache.DataPolicy#REPLICATE} {@link org.apache.geode.cache.Region}.
 *
 * @author John Blum
 * @since 1.9.0
 */
@ReplicateRegion(name = "Accounts", scope = ScopeType.DISTRIBUTED_ACK)
public class ReplicateRegionEntity {

	private String number;

}
