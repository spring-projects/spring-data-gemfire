/*
 * Copyright 2010-2020 the original author or authors.
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

package org.springframework.data.gemfire.eviction;

import org.apache.geode.cache.EvictionAction;
import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.util.ObjectSizer;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;

/**
 * Simple utility class used for defining nested factory-method like definitions w/o polluting the container with useless beans.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.util.ObjectSizer
 */
@SuppressWarnings("unused")
public class EvictionAttributesFactoryBean implements FactoryBean<EvictionAttributes>, InitializingBean {

	protected static final int DEFAULT_LRU_MAXIMUM_ENTRIES = EvictionAttributes.DEFAULT_ENTRIES_MAXIMUM;

	protected static final int DEFAULT_MEMORY_MAXIMUM_SIZE = EvictionAttributes.DEFAULT_MEMORY_MAXIMUM;

	private EvictionAction action = null;

	private EvictionAttributes evictionAttributes;

	private EvictionPolicyType type = EvictionPolicyType.ENTRY_COUNT;

	private Integer threshold = null;

	private ObjectSizer objectSizer = null;

	/**
	 * @inheritDoc
	 */
	public void afterPropertiesSet() {
		evictionAttributes = createAttributes();
	}

	EvictionAttributes createAttributes() {
		switch (type) {
			case HEAP_PERCENTAGE:
				if (threshold != null) {
					throw new IllegalArgumentException("HEAP_PERCENTAGE (LRU_HEAP algorithm) does not support threshold (a.k.a. maximum)!");
				}

				return EvictionAttributes.createLRUHeapAttributes(getObjectSizer(), getAction());
			case MEMORY_SIZE:
				return (threshold != null ? EvictionAttributes.createLRUMemoryAttributes(getThreshold(), getObjectSizer(), getAction())
					: EvictionAttributes.createLRUMemoryAttributes(getObjectSizer(), getAction()));
			case ENTRY_COUNT:
			default:
				return (threshold != null ? EvictionAttributes.createLRUEntryAttributes(getThreshold(), getAction())
					: EvictionAttributes.createLRUEntryAttributes(DEFAULT_LRU_MAXIMUM_ENTRIES, getAction()));
		}
	}

	/**
	 * @inheritDoc
	 */
	public EvictionAttributes getObject() {
		return evictionAttributes;
	}

	/**
	 * @inheritDoc
	 */
	public Class<?> getObjectType() {
		return (evictionAttributes != null ? evictionAttributes.getClass() : EvictionAttributes.class);
	}

	/**
	 * @inheritDoc
	 */
	public boolean isSingleton() {
		return true;
	}

	/**
	 * Sets the action to perform on the Region when Eviction occurs.
	 *
	 * @param action the specified EvictionAction taken on the Region.
	 * @see org.apache.geode.cache.EvictionAction
	 */
	public void setAction(final EvictionAction action) {
		this.action = action;
	}

	/**
	 * Gets the action performed on the Region when Eviction occurs.
	 *
	 * @return the EvictionAction taken on the Region.
	 * @see org.apache.geode.cache.EvictionAction
	 */
	public EvictionAction getAction() {
		return (action != null ? action : EvictionAction.DEFAULT_EVICTION_ACTION);
	}

	/**
	 * Sets the Pivotal GemFire ObjectSizer used in determining object sizes of data stored in the Cache.
	 *
	 * @param objectSizer the ObjectSizer used in sizing object data stored in the Cache.
	 * @see org.apache.geode.cache.util.ObjectSizer
	 */
	public void setObjectSizer(final ObjectSizer objectSizer) {
		this.objectSizer = objectSizer;
	}

	/**
	 * Gets the Pivotal GemFire ObjectSizer used in determining object sizes of data stored in the Cache.
	 *
	 * @return the ObjectSizer used in sizing object data stored in the Cache.
	 * @see org.apache.geode.cache.util.ObjectSizer
	 */
	public ObjectSizer getObjectSizer() {
		return objectSizer;
	}

	/**
	 * Set the threshold used by the LRU algorithm in ENTRY_COUNT and MEMORY_SIZE eviction policy.
	 *
	 * @param threshold an Integer value specifying the threshold used by the LRU algorithm
	 * when enforcing the eviction policy.
	 */
	public void setThreshold(final Integer threshold) {
		this.threshold = threshold;
	}

	/**
	 * Get the threshold used by the LRU algorithm in ENTRY_COUNT and MEMORY_SIZE eviction policy.
	 *
	 * @return an Integer value specifying the threshold used by the LRU algorithm when enforcing the eviction policy.
	 */
	public Integer getThreshold() {
		return threshold;
	}

	/**
	 * Sets the type of eviction policy and algorithm (e.g. LRU on Entry Count, Heap % or Memory Size)
	 * to implement on the Region.
	 *
	 * @param type the type of eviction policy/algorithm to implement on the Region.
	 * @see EvictionPolicyType
	 */
	public void setType(final EvictionPolicyType type) {
		this.type = type;
	}

	/**
	 * Gets the eviction policy and algorithm used by the Region.
	 *
	 * @return the eviction policy and algorithm in use by the Region.
	 * @see EvictionPolicyType
	 */
	public EvictionPolicyType getType() {
		return type;
	}
}
