/*
 * Copyright 2019-2020 the original author or authors.
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
package org.springframework.data.gemfire.support;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.springframework.context.Lifecycle;
import org.springframework.context.SmartLifecycle;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;

/**
 * A Spring {@link Lifecycle} that implements the {@literal Composite software design pattern} composing 1 or more
 * {@link Lifecycle} components as a single, logical, composite {@link Lifecycle} object.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see org.springframework.context.Lifecycle
 * @see org.springframework.context.SmartLifecycle
 * @since 2.2.0
 */
@SuppressWarnings("unused")
public final class CompositeLifecycle implements Iterable<Lifecycle>, SmartLifecycle {

	private final List<Lifecycle> lifecycleComponents = new CopyOnWriteArrayList<>();

	/**
	 * Adds a {@link Lifecycle} object to this composite.
	 *
	 * @param lifecycleComponent {@link Lifecycle} object to add to this composite.
	 * @return a boolean value if the {@link Lifecycle} object is not {@literal null}
	 * and was successfully added to this composite.
	 * @see #remove(Lifecycle)
	 */
	@SuppressWarnings("all")
	public boolean add(@NonNull Lifecycle lifecycleComponent) {
		return lifecycleComponent != null && this.lifecycleComponents.add(lifecycleComponent);
	}

	/**
	 * Returns a boolean value indicating whether this composite contains any {@link Lifecycle} objects.
	 *
	 * @return a boolean value indicating whether this composite contains any {@link Lifecycle} objects.
	 */
	public boolean isEmpty() {
		return this.lifecycleComponents.isEmpty();
	}

	/**
	 * Returns an {@link Iterator} over the {@link Lifecycle} objects contained by this composite.
	 *
	 * @return an {@link Iterator} over the {@link Lifecycle} objects contained by this composite.
	 * @see java.util.Iterator
	 */
	@Override
	public Iterator<Lifecycle> iterator() {
		return Collections.unmodifiableList(this.lifecycleComponents).iterator();
	}

	/**
	 * Removes the given {@link Lifecycle} object from this composite.
	 *
	 * @param lifecycleComponent {@link Lifecycle} object to remove.
	 * @return a boolean if the {@link Lifecycle} object was part of this composite
	 * and was able to be removed successfully.
	 * @see #add(Lifecycle)
	 */
	public boolean remove(@Nullable Lifecycle lifecycleComponent) {
		return this.lifecycleComponents.remove(lifecycleComponent);
	}

	/**
	 * Returns the number of {@link Lifecycle} objects contained by this composite.
	 *
	 * @return an integer value specifying the number of {@link Lifecycle} objects contained by this composite.
	 */
	public int size() {
		return this.lifecycleComponents.size();
	}

	/**
	 * Determines whether any {@link Lifecycle} object contained by this composite is running.
	 *
	 * @return a boolean value indicating whether any {@link Lifecycle} object
	 * contained by this composite is running.
	 * @see org.springframework.context.Lifecycle#isRunning()
	 */
	@Override
	public boolean isRunning() {

		for (Lifecycle lifecycleComponent : this) {
			if (lifecycleComponent.isRunning()) {
				return true;
			}
		}

		return false;
	}

	/**
	 * Starts all {@link Lifecycle} objects contained by this composite.
	 *
	 * @see #stop()
	 */
	@Override
	public void start() {
		this.lifecycleComponents.forEach(Lifecycle::start);
	}

	/**
	 * Stops all {@link Lifecycle} objects contained by this composite.
	 *
	 * @see #start()
	 */
	@Override
	public void stop() {
		this.lifecycleComponents.forEach(Lifecycle::stop);
	}
}
