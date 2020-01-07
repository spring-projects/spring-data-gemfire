/*
 * Copyright 2012-2020 the original author or authors.
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

package org.springframework.data.gemfire.test;

import java.util.Collections;
import java.util.Set;

import org.apache.geode.cache.control.RebalanceFactory;
import org.apache.geode.cache.control.RebalanceOperation;
import org.apache.geode.cache.control.ResourceManager;

/**
 * The StubResourceManager class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class StubResourceManager implements ResourceManager {

	private float criticalHeapPercentage;
	private float criticalOffHeapPercentage;
	private float evictionHeapPercentage;
	private float evictionOffHeapPercentage;

	@Override
	public void setCriticalHeapPercentage(final float heapPercentage) {
		this.criticalHeapPercentage = heapPercentage;
	}

	@Override
	public float getCriticalHeapPercentage() {
		return criticalHeapPercentage;
	}

	@Override
	public void setEvictionHeapPercentage(final float heapPercentage) {
		this.evictionHeapPercentage = heapPercentage;
	}

	@Override
	public float getEvictionHeapPercentage() {
		return this.evictionHeapPercentage;
	}

	@Override
	public RebalanceFactory createRebalanceFactory() {
		throw new UnsupportedOperationException("Not Implemented");
	}

	@Override
	public Set<RebalanceOperation> getRebalanceOperations() {
		return Collections.emptySet();
	}

	@Override
	public void setCriticalOffHeapPercentage(float offHeapPercentage) {
		this.criticalOffHeapPercentage = offHeapPercentage;
	}

	@Override
	public float getCriticalOffHeapPercentage() {
		return this.criticalOffHeapPercentage;
	}

	@Override
	public void setEvictionOffHeapPercentage(float offHeapPercentage) {
		this.evictionOffHeapPercentage = offHeapPercentage;
	}

	@Override
	public float getEvictionOffHeapPercentage() {
		return this.evictionOffHeapPercentage;
	}
}
