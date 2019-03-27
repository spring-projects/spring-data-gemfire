/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.config.support;

/**
 * The GemfireFeature enum is an enumeration of features available in Apache Geode and Pivotal GemFire combined.
 *
 * @author John Blum
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public enum GemfireFeature {

	AEQ,
	BACKUP,
	CACHING,
	CLIENT_SERVER,
	COMPRESSION,
	CONFIGURATION,
	CONSISTENCY,
	CONTINUOUS_QUERY,
	DELTA_PROPAGATION,
	EVENT_HANDLING,
	FUNCTIONS,
	HTTP_SESSION_MANAGEMENT,
	INDEXING,
	JSON,
	LOGGING,
	MANAGEMENT_MONITORING,
	MEMCACHE_SUPPORT,
	NETWORK_PARTITIONING,
	OFF_HEAP,
	PARTITIONING,
	PEER_TO_PEER,
	PERSISTENCE,
	QUERY,
	REGISTER_INTEREST,
	REPLICATION,
	SECURITY,
	SERIALIZATION,
	STATISTICS,
	TRANSACTIONS,
	TUNING,
	WAN

}
