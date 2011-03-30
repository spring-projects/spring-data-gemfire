/*
 * Copyright 2010-2011 the original author or authors.
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

package org.springframework.data.gemfire.support;

import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

import org.springframework.cache.support.AbstractDelegatingCache;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.Region;

/**
 * Spring Framework {@link Cache} implementation using a GemFire {@link Region} underneath.
 * Supports both Gemfire 6.5 and 6.0.
 * 
 * @author Costin Leau
 */
public class GemfireCache<K, V> extends AbstractDelegatingCache<K, V> {

	private static class NoOpLock implements Lock {

		public void lock() {
		}

		public void lockInterruptibly() throws InterruptedException {
		}

		public Condition newCondition() {
			return null;
		}

		public boolean tryLock() {
			return false;
		}

		public boolean tryLock(long time, TimeUnit unit) throws InterruptedException {
			return false;
		}

		public void unlock() {
		}
	};

	private static final Lock NO_OP_LOCK = new NoOpLock();

	private static final boolean hasConcurrentMap = ConcurrentMap.class.isAssignableFrom(Region.class);
	private final Region<K, V> region;
	private final boolean canUseLock;
	private final boolean canUseConcurrentMap;

	/**
	 * Creates a {@link GemFireCache} instance.
	 * 
	 * @param region backing GemFire region
	 */
	public GemfireCache(Region<K, V> region) {
		super(region);
		this.region = region;
		this.canUseLock = region.getAttributes().getScope().isGlobal();
		DataPolicy dataPolicy = region.getAttributes().getDataPolicy();
		this.canUseConcurrentMap = (hasConcurrentMap && (!dataPolicy.isNormal() && !dataPolicy.isEmpty()));
	}

	public String getName() {
		return region.getName();
	}

	public Region<K, V> getNativeCache() {
		return region;
	}

	public V putIfAbsent(K key, V value) {
		if (canUseConcurrentMap) {
			return region.putIfAbsent(key, value);
		}

		// fall back to pre 6.5 API
		Lock lock = (canUseLock ? region.getDistributedLock(key) : NO_OP_LOCK);
		try {
			lock.lock();
			if (!region.containsKey(key)) {
				return region.put(key, value);
			}
			else {
				return region.get(key);
			}
		} finally {
			lock.unlock();
		}
	}

	@SuppressWarnings("unchecked")
	public boolean remove(Object key, Object value) {
		if (canUseConcurrentMap) {
			return region.remove(key, value);
		}

		// fall back to pre 6.5 API
		if (region.containsKey(key)) {
			Lock lock = (canUseLock ? region.getDistributedLock((K) key) : NO_OP_LOCK);
			try {
				lock.lock();

				if (region.get(key).equals(value)) {
					region.remove(key);
					return true;
				}
			} finally {
				lock.unlock();
			}
		}
		return false;
	}

	public boolean replace(K key, V oldValue, V newValue) {
		if (canUseConcurrentMap) {
			return region.replace(key, oldValue, newValue);
		}

		if (region.containsKey(key)) {
			// fall back to pre 6.5 API
			Lock lock = (canUseLock ? region.getDistributedLock(key) : NO_OP_LOCK);

			try {
				lock.lock();

				if (region.get(key).equals(oldValue)) {
					region.put(key, newValue);
					return true;
				}
				else {
					return false;
				}
			} finally {
				lock.unlock();
			}
		}
		return false;
	}

	public V replace(K key, V value) {
		if (canUseConcurrentMap) {
			return region.replace(key, value);
		}

		// fall back to pre 6.5 API
		Lock lock = (canUseLock ? region.getDistributedLock(key) : NO_OP_LOCK);

		try {
			lock.lock();

			if (region.containsKey(key)) {
				return region.put(key, value);
			}
			else {
				return null;
			}
		} finally {
			lock.unlock();
		}
	}
}