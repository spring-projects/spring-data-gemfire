/*
 * Copyright 2019 the original author or authors.
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
package org.springframework.data.gemfire.transaction.event;

import org.apache.geode.cache.TransactionEvent;
import org.apache.geode.cache.TransactionListener;
import org.apache.geode.cache.TransactionWriter;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.util.Assert;

/**
 * The {@link TransactionListenerAdapter} class is an Apache Geode {@link TransactionListener}
 * and {@link TransactionWriter} implementation that publishes the {@link TransactionEvent} to application components
 * and beans declared in the Spring {@link ApplicationContext} using the {@link ApplicationEventPublisher}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.TransactionEvent
 * @see org.apache.geode.cache.TransactionListener
 * @see org.apache.geode.cache.TransactionWriter
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationEventPublisher
 * @since 2.3.0
 */
public class TransactionListenerAdapter implements TransactionListener, TransactionWriter {

	private final ApplicationEventPublisher applicationEventPublisher;

	/**
	 * Constructs a new instance of the {@link TransactionListenerAdapter} initialized with the required
	 * {@link ApplicationEventPublisher} to publish Apache Geode cache {@link TransactionEvent TransactionEvents}
	 * to application declared components and beans in a Spring {@link ApplicationContext}.
	 *
	 * @param applicationEventPublisher {@link ApplicationEventPublisher} used to publish Apache Geode cache
	 * {@link TransactionEvent TransactionEvents}.
	 * @throws IllegalArgumentException if the {@link ApplicationEventPublisher} is {@literal null}.
	 * @see org.springframework.context.ApplicationEventPublisher
	 */
	public TransactionListenerAdapter(ApplicationEventPublisher applicationEventPublisher) {

		Assert.notNull(applicationEventPublisher, "ApplicationEventPublisher must not be null");

		this.applicationEventPublisher = applicationEventPublisher;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void beforeCommit(TransactionEvent event) {

		// NOTE: this will not work because Apache Geode's cache before commit transaction event is only triggered
		// after Spring's AbstractPlatformTransaction.triggerBeforeCommit(:TransactionStatus) method, which is where
		// all application @TransactionalEventListener(phase = TransactionPhase.BEFORE_COMMIT) annotated transaction
		// event handler methods are invoked.

		//this.applicationEventPublisher.publishEvent(TransactionApplicationEvent.of(event));
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterCommit(TransactionEvent event) {
		this.applicationEventPublisher.publishEvent(TransactionApplicationEvent.of(event));
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterFailedCommit(TransactionEvent event) { }

	/**
	 * @inheritDoc
	 */
	@Override
	public void afterRollback(TransactionEvent event) {
		this.applicationEventPublisher.publishEvent(TransactionApplicationEvent.of(event));
	}
}
