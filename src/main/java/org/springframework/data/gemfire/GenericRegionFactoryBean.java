/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.Region;

/**
 * The {@link GenericRegionFactoryBean} class is an extension of the abstract, base {@link PeerRegionFactoryBean} class
 * enabling developers to define a Pivotal GemFire {@link Cache} {@link Region} with defaults.
 *
 * The defaults for DataPolicy is NORMAL and Scope is DISTRIBUTED_NO_ACK, effectively creating a "non-replicate",
 * Distributed Region.
 *
 * This class enables developers to create various non-strongly-typed Regions (e.g. PARTITION, REPLICATE) based on
 * various combinations of the DataPolicy, Scope and Subscription settings as defined in the Region Types section
 * of the Pivotal GemFire User Guide (see link below).  How Pivotal GemFire Regions receive and distribute entry updates
 * is defined in the Storage and Distribution Options section.
 *
 * Note, it is generally better to define strong-typed Regions (e.g. PARTITION with PartitionedRegionFactoryBean)
 * in your applications.  However, different forms of distribution patterns and updates are desired
 * in certain use cases.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.PeerRegionFactoryBean
 * @link https://gemfire.docs.pivotal.io/latest/userguide/index.html#developing/region_options/region_types.html
 * @link https://gemfire.docs.pivotal.io/latest/userguide/index.html#developing/region_options/storage_distribution_options.html
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class GenericRegionFactoryBean<K, V> extends PeerRegionFactoryBean<K, V> {

}
