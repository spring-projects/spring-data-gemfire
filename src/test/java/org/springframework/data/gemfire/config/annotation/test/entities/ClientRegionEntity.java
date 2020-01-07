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

import org.apache.geode.cache.client.ClientRegionShortcut;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.annotation.ClientRegion;

/**
 * {@link ClientRegionEntity} persistent entity stored in the "Users" {@link org.apache.geode.cache.DataPolicy#NORMAL},
 * client {@link org.apache.geode.cache.Region}.
 *
 * @author John Blum
 * @since 1.9.0
 */
@ClientRegion(name = "Sessions", shortcut = ClientRegionShortcut.CACHING_PROXY)
public class ClientRegionEntity {

	@Id
	private String id;

}
