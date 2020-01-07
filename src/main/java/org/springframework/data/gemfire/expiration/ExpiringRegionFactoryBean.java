/*
 * Copyright 2018-2020 the original author or authors.
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

package org.springframework.data.gemfire.expiration;

import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;

import org.springframework.beans.factory.FactoryBean;

/**
 * The {@link ExpiringRegionFactoryBean} interface signifies {@link Region} {@link FactoryBean FactoryBeans}
 * that support Expiration configuration.  That is, {@link Region Region's} capable of expiring both entries
 * as well as the {@link Region} itself.
 *
 * Expiration policies may either be expressed as {@link ExpirationAttributes} or using a {@link CustomExpiry}
 * object enable the application developer to specify custom expiration criteria.
 *
 * Apache Geode and Pivotal GemFire supports both Idle Timeout (TTI) as well as Time-to-Live (TTL) expiration policies
 * at both the {@link Region} level as well as for entries.
 *
 * @author John Blum
 * @see org.apache.geode.cache.CustomExpiry
 * @see org.apache.geode.cache.ExpirationAttributes
 * @see org.apache.geode.cache.Region
 * @since 2.1.0
 */
@SuppressWarnings("unused")
public interface ExpiringRegionFactoryBean<K, V> {

	void setCustomEntryIdleTimeout(CustomExpiry<K, V> customEntryIdleTimeout);

	void setCustomEntryTimeToLive(CustomExpiry<K, V> customEntryTimeToLive);

	void setEntryIdleTimeout(ExpirationAttributes entryIdleTimeout);

	void setEntryTimeToLive(ExpirationAttributes entryTimeToLive);

	void setRegionIdleTimeout(ExpirationAttributes regionIdleTimeout);

	void setRegionTimeToLive(ExpirationAttributes regionTimeToLive);

}
