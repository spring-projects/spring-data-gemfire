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

package org.springframework.data.gemfire.client;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.springframework.util.ObjectUtils;

/**
 * The {@link ClientRegionShortcutWrapper} enum is a Java enumerated type that wraps Pivotal GemFire's
 * {@link ClientRegionShortcut ClientRegionShortcuts} with SDG {@link ClientRegionShortcutWrapper}
 * enumerated values.
 *
 * @author John Blum
 * @see org.apache.geode.cache.DataPolicy
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public enum ClientRegionShortcutWrapper {

	CACHING_PROXY(ClientRegionShortcut.CACHING_PROXY, DataPolicy.NORMAL),
	CACHING_PROXY_HEAP_LRU(ClientRegionShortcut.CACHING_PROXY_HEAP_LRU, DataPolicy.NORMAL),
	CACHING_PROXY_OVERFLOW(ClientRegionShortcut.CACHING_PROXY_OVERFLOW, DataPolicy.NORMAL),
	LOCAL(ClientRegionShortcut.LOCAL, DataPolicy.NORMAL),
	LOCAL_HEAP_LRU(ClientRegionShortcut.LOCAL_HEAP_LRU, DataPolicy.NORMAL),
	LOCAL_OVERFLOW(ClientRegionShortcut.LOCAL_OVERFLOW, DataPolicy.NORMAL),
	LOCAL_PERSISTENT(ClientRegionShortcut.LOCAL_PERSISTENT, DataPolicy.PERSISTENT_REPLICATE),
	LOCAL_PERSISTENT_OVERFLOW(ClientRegionShortcut.LOCAL_PERSISTENT_OVERFLOW, DataPolicy.PERSISTENT_REPLICATE),
	PROXY(ClientRegionShortcut.PROXY, DataPolicy.EMPTY),
	UNSPECIFIED(null, null);

	private final ClientRegionShortcut clientRegionShortcut;

	private final DataPolicy dataPolicy;

	public static ClientRegionShortcutWrapper valueOf(ClientRegionShortcut clientRegionShortcut) {

		for (ClientRegionShortcutWrapper wrapper : values()) {
			if (ObjectUtils.nullSafeEquals(wrapper.getClientRegionShortcut(), clientRegionShortcut)) {
				return wrapper;
			}
		}

		return ClientRegionShortcutWrapper.UNSPECIFIED;
	}

	ClientRegionShortcutWrapper(ClientRegionShortcut clientRegionShortcut, DataPolicy dataPolicy) {
		this.clientRegionShortcut = clientRegionShortcut;
		this.dataPolicy = dataPolicy;
	}

	public ClientRegionShortcut getClientRegionShortcut() {
		return this.clientRegionShortcut;
	}

	public DataPolicy getDataPolicy() {
		return this.dataPolicy;
	}

	public boolean isCaching() {
		return name().contains("CACHING");
	}

	public boolean isHeapLru() {
		return name().contains("HEAP_LRU");
	}

	public boolean isLocal() {
		return name().contains("LOCAL");
	}

	public boolean isOverflow() {
		return name().contains("OVERFLOW");
	}

	public boolean isPersistent() {
		return name().contains("PERSISTENT");
	}

	public boolean isPersistentOverflow() {
		return name().contains("PERSISTENT_OVERFLOW");
	}

	public boolean isProxy() {
		return name().contains("PROXY");
	}
}
