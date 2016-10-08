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

package org.springframework.data.gemfire.support;

import org.apache.geode.cache.client.ClientRegionShortcut;
import org.springframework.util.ObjectUtils;

/**
 * The ClientRegionShortcutWrapper enum is a Java enumerated type that wraps GemFire's ClientRegionShortcuts
 * with Spring Data GemFire ClientRegionShortcutWrapper enumerated values.
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public enum ClientRegionShortcutWrapper {
	CACHING_PROXY(ClientRegionShortcut.CACHING_PROXY),
	CACHING_PROXY_HEAP_LRU(ClientRegionShortcut.CACHING_PROXY_HEAP_LRU),
	CACHING_PROXY_OVERFLOW(ClientRegionShortcut.CACHING_PROXY_OVERFLOW),
	LOCAL(ClientRegionShortcut.LOCAL),
	LOCAL_HEAP_LRU(ClientRegionShortcut.LOCAL_HEAP_LRU),
	LOCAL_OVERFLOW(ClientRegionShortcut.LOCAL_OVERFLOW),
	LOCAL_PERSISTENT(ClientRegionShortcut.LOCAL_PERSISTENT),
	LOCAL_PERSISTENT_OVERFLOW(ClientRegionShortcut.LOCAL_PERSISTENT_OVERFLOW),
	PROXY(ClientRegionShortcut.PROXY),
	UNSPECIFIED(null);

	private final ClientRegionShortcut clientRegionShortcut;

	ClientRegionShortcutWrapper(final ClientRegionShortcut clientRegionShortcut) {
		this.clientRegionShortcut = clientRegionShortcut;
	}

	public static ClientRegionShortcutWrapper valueOf(final ClientRegionShortcut clientRegionShortcut) {
		for (ClientRegionShortcutWrapper wrapper : values()) {
			if (ObjectUtils.nullSafeEquals(wrapper.getClientRegionShortcut(), clientRegionShortcut)) {
				return wrapper;
			}
		}

		return ClientRegionShortcutWrapper.UNSPECIFIED;
	}

	public ClientRegionShortcut getClientRegionShortcut() {
		return clientRegionShortcut;
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
